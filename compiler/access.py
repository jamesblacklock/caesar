from .ast   import ValueExpr, StaticDataType
from .      import block, deref, valueref, letdecl, field, asgn, address, staticdecl, fndecl, coercion
from .types import typesMatch, canPromote, getAlignedSize, PtrType, USize
from .ir    import Swap, Write, Raise, Deref, DerefW, Field, FieldW, DerefField, DerefFieldW, \
                   Fix, Dup, Addr, Imm, Global, Mul, Add, IPTR, FundamentalType
from .scope import ScopeType
from .token import TokenType
from .log   import logError

def createTempSymbol(expr):
	symbol = letdecl.LetDecl.createTemp(expr.span)
	symbol.type = expr.type
	write = SymbolWrite(expr, expr.span)
	write.symbol = symbol
	return (symbol, write)

def createTempTriple(expr):
	(symbol, write) = createTempSymbol(expr)
	read = SymbolRead(expr.span)
	read.symbol = symbol
	read.type = symbol.type
	read.ref = True
	return (symbol, write, read)

class DynOffset:
	def __init__(self, expr, factor):
		self.expr = expr
		self.factor = factor

class SymbolAccess(ValueExpr):
	def __init__(self, span):
		super().__init__(span)
		self.symbol = None
		self.staticOffset = 0
		self.dynOffsets = []
		self.deref = 0
		self.write = False
		self.copy = False
		self.ref = False
		self.field = None
		self.isFieldAccess = False
		self.fieldSpan = None
		self.type = None
		self.dropBlock = None
	
	@staticmethod
	def analyzeSymbolAccess(state, expr, implicitType=None, rvalueImplicitType=None):
		exprs = []
		
		if type(expr) == letdecl.LetDecl:
			exprs.append(expr)
			access = SymbolWrite(expr.expr, expr.span, expr.nameTok.span)
			access.rvalueImplicitType = rvalueImplicitType
			assert expr.dropBlock
			access.dropBlock = expr.dropBlock
			expr.expr = None
		elif type(expr) == asgn.Asgn:
			access = SymbolWrite(expr.rvalue, expr.span, expr.lvalue.span)
			access.rvalueImplicitType = rvalueImplicitType
			assert expr.dropBlock
			access.dropBlock = expr.dropBlock
			expr = expr.lvalue
		else:
			access = SymbolRead(expr.span)
		
		__analyzeSymbolAccess(state, expr, access, exprs, implicitType)
		
		if access.write and access.symbol and access.symbol.dropFn:
			(tempSymbol, tempWrite, tempRead) = createTempTriple(access.rvalue)
			
			access.dropBeforeAssignBlock = block.Block([], access.rvalue.span, noLower=True)
			
			access.rvalue = block.Block([
					tempSymbol, 
					tempWrite, 
					access.dropBeforeAssignBlock, 
					tempRead
				], 
				access.rvalue.span, 
				ScopeType.BLOCK, 
				noLower=True)
		
		if access.symbol == None:
			return access
		
		result = state.analyzeNode(access, implicitType)
		exprBlock = None
		if type(result) == block.Block:
			exprBlock = result
			exprs.extend(result.exprs)
			exprBlock.exprs = exprs
			return exprBlock
		elif len(exprs) > 0:
			exprBlock = block.Block(exprs, access.span, noLower=True)
			exprBlock.type = access.type
			exprs.append(result)
			return exprBlock
		else:
			return result
	
	def pretty(self, output, indent=0):
		closeParen = False
		if not self.write and self.addr:
			output.addPrefix('&')
		elif self.copy:
			output.addPrefix('$copy(')
			closeParen = True
		output.write(self.symbol.name if self.symbol else '???', indent)
		if closeParen:
			output.write(')')
		if self.deref:
			output.write('^' * self.deref)
		if self.field:
			output.write('.' + self.field.name)
		if not self.field and self.staticOffset or self.dynOffsets:
			output.write('[')
			needPlus = False
			if self.staticOffset:
				output.write(str(self.staticOffset))
				needPlus = True
			for dynOff in self.dynOffsets:
				if needPlus:
					output.write(' + ')
				else:
					needPlus = True
				dynOff.expr.pretty(output)
				if dynOff.factor:
					output.write('*{}'.format(dynOff.factor))
			output.write(']')
		if self.write:
			output.write(' = ')
			self.rvalue.pretty(output)
		
class SymbolRead(SymbolAccess):
	def __init__(self, span):
		super().__init__(span)
		self.addr = False
	
	def analyze(access, state, implicitType):
		if type(access.symbol) == staticdecl.StaticDecl:
			access.deref += 1
		
		if access.type == None:
			access.type = access.symbol.type
		if access.field and access.field.isUnionField and not state.scope.allowUnsafe:
			logError(state, fnCall.expr.span, 'reading union fields is unsafe; context is safe')
			
		state.scope.accessSymbol(access)
		
		access.ref = not access.addr and not access.isFieldAccess
		
		if canPromote(access.type, implicitType):
			expr = coercion.Coercion(access, None, access.span, resolvedType=implicitType)
		else:
			expr = access
		
		exprs = []
		
		if access.borrows:
			access.dropBlock = state.scope.scopeLevelDropBlock
			exprs.append(expr)
		else:
			isCopyableDrop = type(access.symbol) == letdecl.LetDecl and access.symbol.dropFn and \
				access.type and access.type.isCopyable
			
			if isCopyableDrop or access.isFieldAccess:
				access.dropBlock = block.Block([], access.span, noLower=True)
				(symbol, write, read) = createTempTriple(expr)
				symbol.type = expr.type
				read.ref = True
				write.type = expr.type
				
				exprs.extend([
					state.analyzeNode(symbol), 
					write, 
					access.dropBlock, 
					read])
				
				state.scope.accessSymbol(write)
				state.scope.accessSymbol(read)
			else:
				exprs.append(expr)
		
		if len(exprs) == 1:
			return exprs[0]
		
		exprBlock = block.Block(exprs, access.span, noLower=True)
		exprBlock.type = access.type
		return exprBlock
	
	def writeIR(expr, state):
		stackTop = False
		if type(expr.symbol) in (staticdecl.StaticDecl, fndecl.FnDecl):
			assert not expr.addr
			state.appendInstr(Global(expr, IPTR, expr.symbol.mangledName))
			stackTop = True
		elif type(expr.symbol) == staticdecl.ConstDecl:
			assert not expr.addr
			expr.symbol.expr.writeIR(state)
			stackTop = True
		else:
			assert type(expr.symbol) in (letdecl.LetDecl, letdecl.FnParam)
			# stackOffset = state.localOffset(expr.symbol)
		
		if expr.isFieldAccess:
			assert not expr.addr
			if expr.deref > 1:
				stackOffset = 0 if stackTop else state.localOffset(expr.symbol)
				state.appendInstr(Dup(expr, stackOffset))
				for _ in range(0, expr.deref - 1):
					state.appendInstr(Deref(expr, IPTR))
				stackTop = True
			
			doAdd = False
			if expr.staticOffset or not expr.dynOffsets:
				state.appendInstr(Imm(expr, IPTR, expr.staticOffset))
				doAdd = True
			for dyn in expr.dynOffsets:
				if doAdd:
					state.appendInstr(Add(expr))
				else:
					doAdd = True
				dyn.expr.writeIR(state)
				if dyn.factor:
					state.appendInstr(Imm(expr, IPTR, dyn.factor))
					state.appendInstr(Mul(expr))
			
			fType = FundamentalType.fromResolvedType(expr.type)
			stackOffset = 1 if stackTop else state.localOffset(expr.symbol)
			if expr.deref:
				state.appendInstr(DerefField(expr, stackOffset, fType))
			else:
				state.appendInstr(Field(expr, stackOffset, fType))
		elif expr.addr:
			stackOffset = 0 if stackTop else state.localOffset(expr.symbol)
			state.appendInstr(Addr(expr, stackOffset))
		else:
			stackOffset = 0 if stackTop else state.localOffset(expr.symbol)
			if expr.copy:
				state.appendInstr(Dup(expr, stackOffset))
			elif stackOffset > 0:
				state.appendInstr(Raise(expr, stackOffset))
			
			if expr.deref:
				for _ in range(0, expr.deref - 1):
					state.appendInstr(Deref(expr, IPTR))
				
				fType = FundamentalType.fromResolvedType(expr.type)
				state.appendInstr(Deref(expr, fType))
	
class SymbolWrite(SymbolAccess):
	def __init__(self, rvalue, span, lvalueSpan=None):
		super().__init__(span)
		self.lvalueSpan = lvalueSpan if lvalueSpan else span
		self.write = True
		self.rvalue = rvalue
		self.dropBlock = block.Block([], span, noLower=True)
		self.dropBeforeAssignBlock = None
		self.rvalueImplicitType = None
		self.hasValue = False
	
	def analyze(access, state, ignoredImplicitType):
		if access.rvalueImplicitType == None:
			access.rvalueImplicitType = access.type
		
		access.rvalue = state.analyzeNode(access.rvalue, access.rvalueImplicitType)
		if access.symbol.type == None:
			access.symbol.type = access.rvalue.type
			access.type = access.rvalue.type
		elif access.rvalue.type:
			if canPromote(access.rvalue.type, access.type):
				access.rvalue = coercion.Coercion(access.rvalue, None, access.span, resolvedType=access.type)
			
			if not typesMatch(access.type, access.rvalue.type):
				logError(state, access.rvalue.span, 
					'expected type {}, found {}'.format(access.type, access.rvalue.type))
		
		if type(access.symbol) == staticdecl.StaticDecl:
			access.deref += 1
		
		access.copy = access.deref != 0
		
		if access.type == None:
			access.type = access.symbol.type
		if access.field and access.field.isUnionField and not state.scope.allowUnsafe:
			logError(state, fnCall.expr.span, 'writing union fields is unsafe; context is safe')
		
		state.scope.accessSymbol(access)
		
		return block.Block([access, access.dropBlock], access.span, noLower=True)
	
	def writeIR(expr, state):
		assert type(expr.symbol) in (letdecl.LetDecl, letdecl.FnParam, staticdecl.StaticDecl)
		
		stackTop = False
		if type(expr.symbol) == staticdecl.StaticDecl:
			state.appendInstr(Global(expr, IPTR, expr.symbol.mangledName))
			stackTop = True
		# else:
			# assert expr.symbol in state.operandsBySymbol
		
		if expr.type.isVoidType:
			expr.rvalue.writeIR(state)
			if not expr.rvalue.type.isVoidType:
				state.appendInstr(Pop(expr))
			return
		
		if expr.isFieldAccess:
			expr.rvalue.writeIR(state)
			doAdd = False
			if expr.staticOffset or not expr.dynOffsets:
				state.appendInstr(Imm(expr, IPTR, expr.staticOffset))
				doAdd = True
			for dyn in expr.dynOffsets:
				dyn.expr.writeIR(state)
				if dyn.factor:
					state.appendInstr(Imm(expr, IPTR, dyn.factor))
					state.appendInstr(Mul(expr))
				if doAdd:
					state.appendInstr(Add(expr))
				else:
					doAdd = True
			stackOffset = 2 if stackTop else state.localOffset(expr.symbol)
			if expr.deref:
				assert expr.deref == 1
				state.appendInstr(DerefFieldW(expr, stackOffset))
			else:
				state.appendInstr(FieldW(expr, stackOffset))
		elif expr.deref:
			assert expr.deref == 1
			stackOffset = 0 if stackTop else state.localOffset(expr.symbol)# stackOffset = state.localOffset(expr.symbol)
			state.appendInstr(Dup(expr, stackOffset))
			expr.rvalue.writeIR(state)
			state.appendInstr(DerefW(expr))
		else:
			expr.rvalue.writeIR(state)
			if state.didBreak:
				return
			
			stackOffset = 0 if expr.symbol not in state.operandsBySymbol else state.localOffset(expr.symbol)
			# if expr.symbol in state.operandsBySymbol:
				# stackOffset = state.localOffset(expr.symbol)
			if stackOffset > 0:
				if expr.symbol.fixed:
					state.appendInstr(Write(expr, stackOffset))
				else:
					state.appendInstr(Swap(expr, stackOffset))
					state.appendInstr(Pop(expr))
			else:
				state.nameTopOperand(expr.symbol)
				if expr.symbol.fixed:
					state.appendInstr(Fix(expr))
			
			if state.loopInfo:
				state.loopInfo.droppedSymbols.discard(expr.symbol)

def _SymbolAccess__analyzeSymbolAccess(state, expr, access, exprs, implicitType=None):
	if type(expr) == letdecl.LetDecl:
		access.symbol = expr
		access.type = access.symbol.type
	elif type(expr) == valueref.ValueRef:
		access.symbol = state.lookupSymbol(expr)
		if access.symbol:
			access.type = access.symbol.type
	elif type(expr) == address.Address and not access.write:
		if implicitType and implicitType.isPtrType:
			implicitType = implicitType.typeAfterDeref()
		addlExprs = []
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, addlExprs, implicitType)
		
		if len(addlExprs) > 0:
			tempSymbol = addlExprs[0]
			tempSymbol.reserve = True
			tempSymbol.fixed = True
			state.scope.beforeScopeLevelExpr.append(tempSymbol)
			exprs.extend(addlExprs[1:])
		
		if access.deref:
			access.deref -= 1
		else:
			assert not access.addr
			access.addr = True
			access.borrows = {access}
		
		if access.type:
			if access.type.isPtrType:
				access.type = PtrType(access.type.baseType, access.type.indLevel + 1, expr.mut)
			else:
				access.type = PtrType(access.type, 1, expr.mut)
	elif type(expr) == deref.Deref:
		if implicitType:
			implicitType = PtrType(implicitType, expr.count, False)
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, exprs, implicitType)
		
		if access.write:
			access.deref = 1
			expr.count -= 1
		else:
			access.deref = expr.count
			expr.count = 0
		
		if expr.count > 0:
			(symbol, write) = createTempSymbol(expr)
			access.symbol = symbol
			exprs.append(state.analyzeNode(symbol))
			exprs.append(state.analyzeNode(write))
			access.type = symbol.type
			assert access.type == None or access.type.isPtrType and access.type.count == 1
		
		if access.type == None:
			pass
		elif not access.type.isPtrType:
			logError(state, expr.expr.span, 'cannot dereference non-pointer type `{}`'.format(access.type))
			access.type = None
		elif access.type.indLevel < access.deref:
			baseType = access.type.baseType
			logError(state, expr.expr.span, 'dereferenced too many times (max: {}; found: {})'.format(
				access.type.indLevel, access.deref))
			access.type = None
		else:
			access.type = access.type.typeAfterDeref(access.deref)
	elif type(expr) == field.Field:
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, exprs)
		if access.type == None:
			return
		
		access.isFieldAccess = True
		
		offset = 0
		fieldInfo = None
		t = access.type
		for tok in expr.path:
			if t.isStructType:
				if tok.content not in t.fieldDict:
					name = 'type `{}`'.format(t.name) if not t.anon else 'struct'
					logError(state, tok.span, '{} has no field `{}`'.format(name, tok.content))
					access.type = None
					return
				
				fieldInfo = t.fieldDict[tok.content]
			elif t.isTupleType:
				fieldIndex = None if tok.type != TokenType.INTEGER else int(tok.content)
				if fieldIndex == None or fieldIndex not in range(0, len(t.fields)):
					name = 'type `{}`'.format(t.name) if not t.anon else 'tuple'
					logError(state, tok.span, '{} has no field `{}`'.format(name, tok.content))
					access.type = None
					return
				
				fieldInfo = t.fields[fieldIndex]
			else:
				logError(state, access.span, 'type `{}` has no fields'.format(t.name))
				access.type = None
				return
			
			offset += fieldInfo.offset
			t = fieldInfo.type
		
		access.staticOffset += offset
		access.type = fieldInfo.type
		if len(access.dynOffsets) == 0:
			access.field = fieldInfo
			access.fieldSpan = expr.path[-1].span
	elif type(expr) == field.Index:
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, exprs)
		expr.index = state.analyzeNode(expr.index, USize)
		
		access.isFieldAccess = True
		
		failed = False
		if not access.type:
			failed = True
		elif not access.type.isArrayType:
			failed = True
			logError(state, expr.expr.span, 
				'base of index expression must be an array type (found {})'.format(access.type))
		elif expr.index.type and expr.index.type != USize:
			failed = True
			logError(state, expr.index.span, 'index must be type usize (found {})'.format(expr.index.type))
		
		if failed:
			return
		
		staticIndex = expr.index.staticEval(state)
		if staticIndex:
			assert staticIndex.dataType == StaticDataType.INT
			if staticIndex.data >= access.type.count:
				logError(state, expr.expr.span, 
					'index {} out of range for array of size {}'.format(staticIndex.data, access.type.count))
			
			fieldInfo = access.type.fields[staticIndex.data]
			access.staticOffset += fieldInfo.offset
			if len(access.dynOffsets) == 0:
				access.field = fieldInfo
				access.fieldSpan = expr.index.span
		else:
			factor = getAlignedSize(access.type.baseType)
			access.dynOffsets.append(DynOffset(expr.index, factor if factor > 1 else None))
		
		access.type = access.type.baseType
	else:
		(symbol, write) = createTempSymbol(expr)
		access.symbol = symbol
		exprs.append(state.analyzeNode(symbol))
		exprs.append(state.analyzeNode(write))
		access.type = access.symbol.type
