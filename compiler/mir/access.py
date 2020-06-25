from ..ast.ast    import ValueExpr
from .mir         import MIR, StaticDataType
from ..ast        import block, deref, valueref, localdecl, field, asgn, address
from ..not_done   import staticdecl, fndecl
from ..types      import typesMatch, tryPromote, shapesMatch, getAlignedSize, PtrType, USize
from ..ir         import Swap, Pop, Write, Raise, Deref, DerefW, Field, FieldW, DerefField, DerefFieldW, \
                         Fix, Dup, Addr, Imm, Global, Mul, Add, IPTR, FundamentalType
from ..scope      import ScopeType
from ..token      import TokenType
from ..log        import logError
from .localsymbol import LocalSymbol

def createTempSymbol(*exprs):
	symbol = LocalSymbol.createTemp(exprs[0].span)
	symbol.type = exprs[0].type
	writes = []
	for expr in exprs:
		write = SymbolWrite(expr, expr.span)
		write.symbol = symbol
		writes.append(write)
	return (symbol, *writes)

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

class SymbolAccess(MIR):
	def __init__(self, span, hasValue):
		super().__init__(span, hasValue)
		self.symbol = None
		self.isBorrowed = False
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
	def read(state, expr):
		(tempSymbol, tempWrite, tempRead) = createTempTriple(expr)
		state.analyzeNode(tempSymbol)
		state.analyzeNode(tempWrite)
		return tempRead
	
	@staticmethod
	def noop(symbol, dropBlock, span):
		ref = SymbolRead(span)
		ref.symbol = symbol
		ref.type = symbol.type
		ref.noop = True
		ref.dropBlock = dropBlock
		return ref
	
	@staticmethod
	def analyzeSymbolAccess(state, expr, implicitType=None, rvalueImplicitType=None):
		if type(expr) == asgn.Asgn:
			access = SymbolWrite(expr.rvalue, expr.span, expr.lvalue.span)
			access.rvalueImplicitType = rvalueImplicitType
			expr = expr.lvalue
		elif type(expr) == SymbolWrite:
			access = expr
		else:
			access = SymbolRead(expr.span)
		
		__analyzeSymbolAccess(state, expr, access, implicitType)
		
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
		
		return state.analyzeNode(access, implicitType)
	
	def __str__(self):
		if not self.write and self.noop:
			return '$noop({})'.format(self.symbol.name if self.symbol else '???')
		
		s = ''
		closeParen = False
		if not self.write and self.addr:
			s += '&'
		elif self.copy:
			s += '$copy('
			closeParen = True
		s += self.symbol.name if self.symbol else '???'
		if closeParen:
			s += ')'
		if self.deref:
			s += '^' * self.deref
		if self.field:
			s += '.' + self.field.name
		if not self.field and self.staticOffset or self.dynOffsets:
			s += '['
			needPlus = False
			if self.staticOffset:
				s += str(self.staticOffset)
				needPlus = True
			for dynOff in self.dynOffsets:
				if needPlus:
					s += ' + '
				else:
					needPlus = True
				s += str(dynOff.expr)
				if dynOff.factor:
					s += '*{}'.format(dynOff.factor)
			s += ']'
		if self.write:
			s += ' = ' + str(self.rvalue)
		return s

class SymbolRead(SymbolAccess):
	def __init__(self, span):
		super().__init__(span, True)
		self.lvalueSpan = span
		self.addr = False
		self.borrow = False
		self.noop = False
		self.hasValue = True
		self.typeModifiers = None
	
	def moveToClone(self):
		other = SymbolRead(self.span)
		other.symbol = self.symbol
		other.staticOffset = self.staticOffset
		other.dynOffsets = self.dynOffsets
		other.deref = self.deref
		other.write = self.write
		other.copy = self.copy
		other.ref = self.ref
		other.field = self.field
		other.isFieldAccess = self.isFieldAccess
		other.fieldSpan = self.fieldSpan
		other.type = self.type
		other.lvalueSpan = self.lvalueSpan
		other.addr = self.addr
		other.borrow = self.borrow
		other.borrows = self.borrows
		other.contracts = self.contracts
		
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
		self.addr = False
		self.borrow = False
		self.borrows = set()
		self.contracts = None
		assert not self.isBorrowed
		
		return other
	
	def analyze(access, state, implicitType):
		if type(access.symbol) == staticdecl.StaticDecl:
			access.deref += 1
		
		if access.type == None:
			access.type = access.symbol.type
		if access.isFieldAccess and access.field and access.field.isUnionField and not state.scope.allowUnsafe:
			logError(state, access.span, 'reading union fields is unsafe; context is safe')
		
		access.ref = not access.addr and not access.isFieldAccess
		
		# if canPromote(access.type, implicitType):
		# 	expr = coercion.AsExpr(access, None, access.span, resolvedType=implicitType)
		# else:
		# 	expr = access
		access = tryPromote(state, access, implicitType)
		
		
		isCopyableDrop = type(access.symbol) == LocalSymbol and access.symbol.dropFn and \
			access.type and access.type.isCopyable
		
		if isCopyableDrop or access.isFieldAccess:
			access.dropBlock = state.scope.dropBlock
			(symbol, write, read) = createTempTriple(access)
			symbol.type = access.type
			read.ref = True
			write.type = access.type
			write.analyzeRValue = False
			
			state.analyzeNode(symbol)
			state.analyzeNode(write)
			access = read
		
		access.contracts = access.symbol.contracts
		access.dropBlock = state.scope.dropBlock
		return access
	
	def checkFlow(self, scope):
		for off in self.dynOffsets:
			off.expr.checkFlow(scope)
		scope.accessSymbol(self)
	
	def writeIR(expr, state):
		if expr.noop:
			return
		
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
			assert type(expr.symbol) == LocalSymbol
			# stackOffset = state.localOffset(expr.symbol)
		
		if expr.isFieldAccess:
			if expr.addr:
				stackOffset = 0 if stackTop else state.localOffset(expr.symbol)
				state.appendInstr(Addr(expr, stackOffset))
			elif expr.deref > 1:
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
			elif expr.addr:
				assert doAdd
				state.appendInstr(Add(expr))
			else:
				state.appendInstr(Field(expr, stackOffset, fType))
		elif expr.addr:
			stackOffset = 0 if stackTop else state.localOffset(expr.symbol)
			state.appendInstr(Addr(expr, stackOffset))
		else:
			stackOffset = 0 if stackTop else state.localOffset(expr.symbol)
			if expr.copy or expr.isBorrowed:
				state.appendInstr(Dup(expr, stackOffset))
			elif stackOffset > 0:
				state.appendInstr(Raise(expr, stackOffset))
			
			if expr.deref:
				for _ in range(0, expr.deref - 1):
					state.appendInstr(Deref(expr, IPTR))
				
				fType = FundamentalType.fromResolvedType(expr.type)
				state.appendInstr(Deref(expr, fType))
			elif expr.staticOffset:
				state.appendInstr(Imm(expr, IPTR, expr.staticOffset))
				state.appendInstr(Add(expr))

class SymbolWrite(SymbolAccess):
	def __init__(self, rvalue, span, lvalueSpan=None):
		super().__init__(span, False)
		self.lvalueSpan = lvalueSpan if lvalueSpan else span
		self.write = True
		self.rvalue = rvalue
		self.dropBeforeAssignBlock = None
		self.rvalueImplicitType = None
		self.hasValue = False
		self.analyzeRValue = True
	
	def analyze(access, state, ignoredImplicitType):
		if access.rvalueImplicitType == None:
			access.rvalueImplicitType = access.type
		
		if access.analyzeRValue:
			access.rvalue = state.analyzeNode(access.rvalue, access.rvalueImplicitType, isWrite=True)
		access.symbol.contracts = access.rvalue.contracts
		if access.symbol.type == None:
			access.symbol.type = access.rvalue.type
			access.type = access.rvalue.type
		elif access.type and access.rvalue.type:
			# if canPromote(access.rvalue.type, access.type):
			# 	access.rvalue = coercion.AsExpr(access.rvalue, None, access.span, resolvedType=access.type)
			access.rvalue = tryPromote(state, access.rvalue, access.rvalueImplicitType)
			
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
		
		access.dropBlock = state.scope.dropBlock
		access.rvalue.dropBlock = state.scope.dropBlock
		return access
	
	def checkFlow(self, scope):
		self.rvalue.checkFlow(scope)
		for off in self.dynOffsets:
			off.expr.checkFlow(scope)
		scope.accessSymbol(self)
	
	def writeIR(expr, state):
		assert type(expr.symbol) in (LocalSymbol, staticdecl.StaticDecl)
		
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
					state.appendInstr(Fix(expr, 0))
			
			if state.loopInfo:
				state.loopInfo.droppedSymbols.discard(expr.symbol)

def _SymbolAccess__analyzeSymbolAccess(state, expr, access, implicitType=None):
	# if type(expr) == localdecl.LetDecl:
	# 	access.symbol = expr
	# 	access.type = access.symbol.type
	# el
	if type(expr) == valueref.ValueRef:
		symbolTable = implicitType.symbolTable if implicitType else None
		access.symbol = state.lookupSymbol(expr.path, symbolTable, inValuePosition=True)
		if access.symbol:
			if implicitType and access.symbol.type and access.symbol.type.canChangeTo(implicitType):
				access.symbol.type = implicitType
			
			t = access.symbol.type
			if access.symbol in state.scope.contracts:
				contract = state.scope.contracts[access.symbol]
				if len(contract.variants) == 1:
					t = contract.variants[0].type
					if contract.indLevel > 0:
						t = PtrType(t, contract.indLevel, contract.symbol.mut)
			
			access.type = t
	elif type(expr) == valueref.Borrow:
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, implicitType)
		if access.type and not access.type.isOwnedType:
			logError(state, expr.span, 'type `{}` cannot be borrowed'.format(access.type))
		else:
			access.type = access.type.baseType
			access.borrow = True
			access.copy = True
			access.borrows = {access}
	elif type(expr) == address.Address and not access.write:
		if implicitType and implicitType.isPtrType:
			implicitType = implicitType.typeAfterDeref()
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, implicitType)
		
		addrExpr = access.moveToClone()
		
		if addrExpr.deref:
			addrExpr.deref -= 1
			addrExpr.copy = True
			if addrExpr.isFieldAccess:
				addrExpr.isFieldAccess = False
				# addrExpr.field = None
				addrExpr.borrows = {addrExpr}
		else:
			assert not addrExpr.addr
			addrExpr.addr = True
			addrExpr.borrows = {addrExpr}
		
		if addrExpr.type:
			if addrExpr.type.isPtrType:
				addrExpr.type = PtrType(addrExpr.type.baseType, addrExpr.type.indLevel + 1, expr.mut)
			else:
				addrExpr.type = PtrType(addrExpr.type, 1, expr.mut)
		
		(tempSymbol, tempWrite) = createTempSymbol(addrExpr)
		tempSymbol.reserve = True
		state.analyzeNode(tempSymbol)
		state.analyzeNode(tempWrite)
		
		access.symbol = tempSymbol
		access.type = access.symbol.type
		access.copy = True
			
	elif type(expr) == deref.Deref:
		if implicitType:
			implicitType = PtrType(implicitType, expr.count, False)
		
		if type(expr.expr) != valueref.ValueRef:
			(symbol, write) = createTempSymbol(expr.expr)
			access.symbol = symbol
			state.analyzeNode(symbol)
			state.analyzeNode(write)
			access.type = access.symbol.type
		else:
			_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, implicitType)
		
		derefCount = expr.count
		if access.write:
			access.deref = 1
			derefCount -= 1
		else:
			access.deref = derefCount
			derefCount = 0
		
		if derefCount > 0:
			(symbol, write) = createTempSymbol(expr)
			access.symbol = symbol
			state.analyzeNode(symbol)
			state.analyzeNode(write)
			access.type = symbol.type
			assert access.type == None or access.type.isPtrType and access.type.count == 1
		
		t = access.type
		if t and t.isOwnedType:
			t = t.baseType
		
		if t == None:
			pass
		elif not t.isPtrType:
			logError(state, expr.expr.span, 'cannot dereference non-pointer type `{}`'.format(access.type))
			access.type = None
		elif t.indLevel < access.deref:
			baseType = t.baseType
			logError(state, expr.expr.span, 'dereferenced too many times (max: {}; found: {})'.format(
				t.indLevel, access.deref))
			access.type = None
		else:
			access.type = t.typeAfterDeref(access.deref)
	elif type(expr) == field.Field:
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access)
		if access.type == None:
			return
		
		access.isFieldAccess = True
		
		offset = 0
		fieldInfo = None
		t = access.type
		for tok in expr.path:
			name = t.name
			if t.isEnumType:
				t = t.structType
				anon = False
			else:
				anon = t.isCompositeType and t.anon
			
			if t.isStructType:
				if tok.content not in t.fieldDict:
					name = 'struct' if anon else 'type `{}`'.format(name)
					logError(state, tok.span, '{} has no field `{}`'.format(name, tok.content))
					access.type = None
					return
				
				fieldInfo = t.fieldDict[tok.content]
			elif t.isTupleType:
				fieldIndex = None if tok.type != TokenType.INTEGER else int(tok.content)
				if fieldIndex == None or fieldIndex not in range(0, len(t.fields)):
					name = 'tuple' if anon else 'type `{}`'.format(name)
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
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access)
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
		state.analyzeNode(symbol)
		state.analyzeNode(write)
		access.type = access.symbol.type
