from enum           import Enum
from .mir           import MIR, StaticDataType
from ..ast          import deref, valueref, field, asgn, address
from ..types        import typesMatch, tryPromote, getAlignedSize, PtrType, USize
from ..             import ir
from ..log          import logError
from ..symbol.local import Local

class AccessType(Enum):
	REF_READ = 'REF_READ'
	REF_WRITE = 'REF_WRITE'
	REF_ADDR = 'REF_ADDR'
	FIELD_READ = 'FIELD_READ'
	FIELD_WRITE = 'FIELD_WRITE'
	FIELD_ADDR = 'FIELD_ADDR'
	DEREF_READ = 'DEREF_READ'
	DEREF_WRITE = 'DEREF_WRITE'
	DEREF_FIELD_READ = 'DEREF_FIELD_READ'
	DEREF_FIELD_WRITE = 'DEREF_FIELD_WRITE'

def createTempSymbol(*exprs):
	symbol = Local.createTemp(exprs[0].span)
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
	read.accessType = AccessType.REF_READ
	return (symbol, write, read)

class DynOffset:
	def __init__(self, expr, baseType):
		self.expr = expr
		self.baseType = baseType

class SymbolAccess(MIR):
	def __init__(self, span, hasValue):
		super().__init__(span, hasValue)
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
		self.dropPoint = None
		self.accessType = None
		self.typeFailed = False
	
	@staticmethod
	def read(state, expr):
		if type(expr) == SymbolRead and expr.ref:
			return expr
		
		(tempSymbol, tempWrite, tempRead) = createTempTriple(expr)
		state.block.decl(tempSymbol)
		state.analyzeNode(tempWrite)
		tempRead.type = tempSymbol.type
		return tempRead
	
	@staticmethod
	def readSymbol(symbol, span):
		read = SymbolRead(span)
		read.symbol = symbol
		read.type = symbol.type
		read.ref = True
		read.accessType = AccessType.REF_READ
		return read
	
	@staticmethod
	def write(state, symbol, expr):
		write = SymbolWrite(expr, expr.span)
		write.symbol = symbol
		state.analyzeNode(write)
		read = SymbolRead(expr.span)
		read.symbol = symbol
		read.type = symbol.type
		read.ref = True
		read.accessType = AccessType.REF_READ
		return read
	
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
		
		if access.write and access.symbol and (access.symbol.dropFn or access.deref or access.isFieldAccess):
			(tempSymbol, tempWrite, tempRead) = createTempTriple(access.rvalue)
			
			state.decl(tempSymbol)
			tempWrite.rvalueImplicitType = access.type
			state.analyzeNode(tempWrite)
			state.appendDropPoint()
			tempRead.type = tempSymbol.type
			access.rvalue = tempRead
		
		if access.symbol == None:
			return access
		
		return state.analyzeNode(access, implicitType)
	
	def __str__(self):
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
				s += str(dynOff.expr) + '*{}'.format(dynOff.baseType.name)
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
		self.hasValue = True
		self.typeModifiers = None
		self.leakOwned = False
	
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
		
		return other
	
	def analyze(self, state, implicitType):
		if self.symbol == None or self.symbol.type == None:
			return self
		
		self.symbol.unused = False
		
		if self.type == None and not self.typeFailed:
			self.type = self.symbol.type
		if self.isFieldAccess and self.field and self.field.isUnionField and not state.scope.allowUnsafe:
			logError(state, self.span, 'reading union fields is unsafe; context is safe')
		
		if not self.addr:
			self.ref = not self.isFieldAccess
			if self.symbol.isLocal:
				self.copy = self.type.isCopyable and not self.leakOwned
		
		if self.ref:
			if self.deref:
				self.accessType = AccessType.DEREF_READ
			else:
				self.accessType = AccessType.REF_READ
		elif self.addr:
			if self.isFieldAccess:
				self.accessType = AccessType.FIELD_ADDR
			else:
				self.accessType = AccessType.REF_ADDR
		else:
			assert self.isFieldAccess
			if self.deref:
				self.accessType = AccessType.DEREF_FIELD_READ
			else:
				self.accessType = AccessType.FIELD_READ
		
		self.dropPoint = state.dropPoint
		self = tryPromote(state, self, implicitType)
		
		isCopyableDrop = self.symbol.isLocal and self.symbol.dropFn and self.type and self.type.isCopyable
		
		if isCopyableDrop or self.isFieldAccess:
			(symbol, write, read) = createTempTriple(self)
			write.analyzeRValue = False
			
			state.decl(symbol)
			state.analyzeNode(write)
			read.type = write.type
			self = read
			self.dropPoint = state.dropPoint
		
		self.contracts = self.symbol.contracts
		
		return self
	
	def commit(self, state):
		for off in self.dynOffsets:
			state.refType(off.baseType)
			off.expr.commit(state)
		state.access(self)
	
	def staticEval(self, state):
		if not self.ref or self.deref:
			return None
		return state.staticRead(self.symbol)
	
	def writeIR(self, state):
		if self.type.isVoidType:
			return
		
		symbol = state.ast.genericInc[self.symbol] if self.symbol.isGeneric else self.symbol
		
		stackTop = False
		if self.symbol.isStatic or self.symbol.isFn:
			assert not self.addr
			state.appendInstr(ir.Global(self, ir.IPTR, self.symbol.mangledName))
			stackTop = True
		elif self.symbol.isConst:
			assert not self.addr
			ir.writeStaticValueIR(state, self, symbol.staticValue)
			stackTop = True
		else:
			assert type(self.symbol) == Local
		
		if self.isFieldAccess:
			if self.addr:
				stackOffset = 0 if stackTop else state.localOffset(self.symbol)
				state.appendInstr(ir.Addr(self, stackOffset))
			elif self.deref > 1:
				stackOffset = 0 if stackTop else state.localOffset(self.symbol)
				state.appendInstr(ir.Dup(self, stackOffset))
				for _ in range(0, self.deref - 1):
					state.appendInstr(ir.Deref(self, ir.IPTR))
				stackTop = True
			
			doAdd = False
			if self.staticOffset or not self.dynOffsets:
				state.appendInstr(ir.Imm(self, ir.IPTR, self.staticOffset))
				doAdd = True
			for dyn in self.dynOffsets:
				if doAdd:
					state.appendInstr(ir.Add(self))
				else:
					doAdd = True
				dyn.expr.writeIR(state)
				factor = getAlignedSize(dyn.baseType)
				if factor > 1:
					state.appendInstr(ir.Imm(self, ir.IPTR, factor))
					state.appendInstr(ir.Mul(self))
			
			fType = ir.FundamentalType.fromResolvedType(state.ast, self.type)
			stackOffset = 1 if stackTop else state.localOffset(self.symbol)
			if self.deref:
				state.appendInstr(ir.DerefField(self, stackOffset, fType))
			elif self.addr:
				assert doAdd
				state.appendInstr(ir.Add(self))
			else:
				state.appendInstr(ir.Field(self, stackOffset, fType))
		elif self.addr:
			stackOffset = 0 if stackTop else state.localOffset(self.symbol)
			state.appendInstr(ir.Addr(self, stackOffset))
		else:
			stackOffset = 0 if stackTop else state.localOffset(self.symbol)
			if self.copy:
				state.appendInstr(ir.Dup(self, stackOffset))
			elif stackOffset > 0:
				state.appendInstr(ir.Raise(self, stackOffset))
			
			if self.deref:
				for _ in range(0, self.deref - 1):
					state.appendInstr(ir.Deref(self, ir.IPTR))
				
				fType = ir.FundamentalType.fromResolvedType(state.ast, self.type)
				state.appendInstr(ir.Deref(self, fType))
			elif self.staticOffset:
				state.appendInstr(ir.Imm(self, ir.IPTR, self.staticOffset))
				state.appendInstr(ir.Add(self))

class SymbolWrite(SymbolAccess):
	def __init__(self, rvalue, span, lvalueSpan=None):
		super().__init__(span, False)
		self.lvalueSpan = lvalueSpan if lvalueSpan else span
		self.write = True
		self.rvalue = rvalue
		self.rvalueImplicitType = None
		self.hasValue = False
		self.analyzeRValue = True
		self.beforeWriteDropPoint = None
	
	def analyze(self, state, ignoredImplicitType):
		self.symbol.unused = False
		if self.type == None and not self.typeFailed:
			self.type = self.symbol.type
		if self.rvalueImplicitType == None:
			self.rvalueImplicitType = self.type
		
		if self.analyzeRValue:
			self.rvalue = state.analyzeNode(self.rvalue, self.rvalueImplicitType, isRValue=True)
		
		if self.rvalue:
			self.symbol.contracts = self.rvalue.contracts
			if self.symbol.type == None:
				self.symbol.type = self.rvalue.type
				self.type = self.rvalue.type
				self.symbol.checkDropFn(state)
			elif self.type and self.rvalue.type:
				self.rvalue = tryPromote(state, self.rvalue, self.rvalueImplicitType)
				
				if not typesMatch(self.type, self.rvalue.type):
					logError(state, self.rvalue.span, 
						'expected type {}, found {}'.format(self.type, self.rvalue.type))
		
		self.copy = self.deref != 0
		
		if self.type == None:
			self.type = self.symbol.type
		if self.field and self.field.isUnionField and not state.scope.allowUnsafe:
			logError(state, self.span, 'writing union fields is unsafe; context is safe')
		
		if self.isFieldAccess:
			if self.deref:
				self.accessType = AccessType.DEREF_FIELD_WRITE
			else:
				self.accessType = AccessType.FIELD_WRITE
		elif self.deref:
			self.accessType = AccessType.DEREF_WRITE
		else:
			self.accessType = AccessType.REF_WRITE
		
		if self.deref or self.field:
			self.beforeWriteDropPoint = state.dropPoint
			state.appendDropPoint()
		
		self.dropPoint = state.dropPoint
		if self.rvalue:
			self.rvalue.dropPoint = state.dropPoint
		
		state.append(self)
	
	def commit(self, state):
		if self.rvalue == None:
			return
		
		self.rvalue.commit(state)
		for off in self.dynOffsets:
			state.refType(off.baseType)
			off.expr.commit(state)
		state.access(self)
		self.staticSideEffects(state)
	
	def staticSideEffects(self, state):
		if self.deref or self.isFieldAccess:
			return False
		staticValue = self.rvalue.staticEval(state)
		if staticValue:
			state.staticWrite(self.symbol, staticValue)
			return True
		return False
	
	def writeIR(self, state):
		assert self.symbol.isLocal or self.symbol.isStatic
		
		if self.rvalue == None:
			return
		
		stackTop = False
		if self.symbol.isStatic:
			state.appendInstr(ir.Global(self, ir.IPTR, self.symbol.mangledName))
			stackTop = True
		# else:
			# assert self.symbol in state.operandsBySymbol
		
		if self.type.isVoidType:
			self.rvalue.writeIR(state)
			if not self.rvalue.type.isVoidType:
				state.appendInstr(ir.Pop(self))
			return
		
		if self.isFieldAccess:
			self.rvalue.writeIR(state)
			doAdd = False
			if self.staticOffset or not self.dynOffsets:
				state.appendInstr(ir.Imm(self, ir.IPTR, self.staticOffset))
				doAdd = True
			for dyn in self.dynOffsets:
				dyn.expr.writeIR(state)
				t = dyn.baseType
				if t.isGenericType:
					t = state.ast.genericInc[dyn.baseType.symbol].type
				factor = getAlignedSize(t)
				if factor > 1:
					state.appendInstr(ir.Imm(self, ir.IPTR, factor))
					state.appendInstr(ir.Mul(self))
				if doAdd:
					state.appendInstr(ir.Add(self))
				else:
					doAdd = True
			stackOffset = 2 if stackTop else state.localOffset(self.symbol)
			if self.deref:
				assert self.deref == 1
				state.appendInstr(ir.DerefFieldW(self, stackOffset))
			else:
				state.appendInstr(ir.FieldW(self, stackOffset))
		elif self.deref:
			assert self.deref == 1
			stackOffset = 0 if stackTop else state.localOffset(self.symbol)
			state.appendInstr(ir.Dup(self, stackOffset))
			self.rvalue.writeIR(state)
			state.appendInstr(ir.DerefW(self))
		else:
			self.rvalue.writeIR(state)
			# if state.didBreak:
				# return
			
			stackOffset = 0 if self.symbol not in state.operandsBySymbol else state.localOffset(self.symbol)
			if stackOffset > 0:
				if self.symbol.fixed:
					state.appendInstr(ir.Write(self, stackOffset))
				else:
					state.appendInstr(ir.Swap(self, stackOffset))
					state.appendInstr(ir.Pop(self))
			else:
				state.nameTopOperand(self.symbol)
				if self.symbol.fixed:
					state.appendInstr(ir.Fix(self, 0))

def _SymbolAccess__analyzeSymbolAccess(state, expr, access, implicitType=None):
	from ..attrs        import invokeAttrs
	invokeAttrs(state, expr)
	
	if type(expr) == valueref.ValueRef:
		symbolTable = implicitType.symbolTable if implicitType else None
		access.symbol = state.lookupSymbol(expr.path, symbolTable, inValuePosition=True)
		if access.symbol:
			if implicitType and access.symbol.type and access.symbol.type.canChangeTo(implicitType):
				access.symbol.type = implicitType
				access.symbol.checkDropFn(state)
			
			t = access.symbol.type
			if access.symbol in state.scope.contracts:
				contract = state.scope.contracts[access.symbol]
				if len(contract.variants) == 1:
					t = contract.variants[0].type
					if contract.indLevel > 0:
						t = PtrType(t, contract.indLevel, contract.symbol.mut)
			
			if expr.leakOwned:
				if not state.scope.allowUnsafe:
					logError(state, expr.span, 'cannot leak owned data in safe context')
				if t.isOwnedType:
					access.leakOwned = True
					t = t.baseType
			
			access.type = t
			if access.symbol.isStatic:
				staticRead = SymbolRead(access.span)
				staticRead.symbol = access.symbol
				staticRead.type = PtrType(access.symbol.type, 1, False)
				staticRead.ref = True
				(symbol, write) = createTempSymbol(staticRead)
				state.decl(symbol)
				state.analyzeNode(write)
				access.symbol = symbol
				access.deref = 1
	elif type(expr) == valueref.Borrow:
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access, implicitType)
		if not access.type:
			return
		elif not access.type.isOwnedType:
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
		
		if access.symbol and access.symbol.type == None:
			logError(state, expr.expr.span, 'cannot take address of `{}`: its type is unknown'.format(access.symbol.name))
		
		addrExpr = access.moveToClone()
		
		if addrExpr.deref:
			addrExpr.deref -= 1
			if addrExpr.isFieldAccess:
				addrExpr.isFieldAccess = False
				addrExpr.borrows = {addrExpr}
				addrExpr.copy = True
			else:
				addrExpr.copy = addrExpr.deref > 0
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
		state.decl(tempSymbol)
		state.analyzeNode(tempWrite)
		
		access.symbol = tempSymbol
		access.type = access.symbol.type
			
	elif type(expr) == deref.Deref:
		if implicitType:
			implicitType = PtrType(implicitType, expr.count, False)
		
		if type(expr.expr) != valueref.ValueRef:
			(symbol, write) = createTempSymbol(expr.expr)
			access.symbol = symbol
			state.decl(symbol)
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
			state.decl(symbol)
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
		for fieldName in expr.path:
			# name = t.name
			# if t.isEnumType:
			# 	t = t.structType
			# 	anon = False
			# else:
			# 	anon =  and t.anon
			
			if not t.isCompositeType:
				logError(state, access.span, 'type `{}` has no fields'.format(t.name))
				access.type = None
				return
			elif fieldName.content not in t.fieldDict:
				logError(state, fieldName.span, 'type `{}` has no field `{}`'.format(t.name, fieldName.content))
				access.type = None
				return
			
			fieldInfo = t.fieldDict[fieldName.content]
			
			privateAccess = False
			if expr.privateAccess:
				if not state.scope.allowUnsafe:
					logError(state, expr.span, 'cannot violate field privacy data in safe context')
				privateAccess = True
			
			pub = (fieldInfo.pub and fieldInfo.mut) if access.write else fieldInfo.pub
			if not (pub or privateAccess):
				symbolMod = t.symbol.mod
				while symbolMod.transparent:
					symbolMod = symbolMod.parent
				mod = state.mod
				while True:
					if mod == symbolMod:
						break
					mod = mod.parent
					if mod == None:
						if access.write and fieldInfo.pub:
							logError(state, fieldName.span, 'field `{}` is read-only'.format(fieldName.content))
						else:
							logError(state, fieldName.span, 'field `{}` is private'.format(fieldName.content))
						break
			
			offset += fieldInfo.offset
			t = fieldInfo.type
		
		if expr.leakOwned:
			if not state.scope.allowUnsafe:
				logError(state, expr.span, 'cannot leak owned data in safe context')
			if t.isOwnedType:
				access.leakOwned = True
				t = t.baseType
		
		access.staticOffset += offset
		access.type = t
		if len(access.dynOffsets) == 0:
			access.field = fieldInfo
			access.fieldSpan = expr.path[-1].span
	elif type(expr) == field.Index:
		_SymbolAccess__analyzeSymbolAccess(state, expr.expr, access)
		index = state.analyzeNode(expr.index, USize)
		failed = index == None
		
		access.isFieldAccess = True
		
		isArr = False
		isVec = False
		if state.mod.arrMod and access.type and access.type.isStructType and access.type.symbol.paramType:
			isArr = access.type.symbol.paramType == state.mod.arrMod.Arr
			isVec = access.type.symbol.paramType == state.mod.arrMod.Vec
		
		count = None
		fields = None
		
		if not access.type:
			failed = True
		elif isArr:
			count = access.type.symbolTable['Len'].staticValue.data
			fields = access.type.fields[1].type.typeAfterDeref().fields
			
			dataPtr = access.moveToClone()
			dataPtr.field = dataPtr.type.fields[1]
			dataPtr.fieldSpan = dataPtr.span
			dataPtr.staticOffset += dataPtr.field.offset
			dataPtr.type = dataPtr.type.fields[1].type
			
			(tempSymbol, tempWrite) = createTempSymbol(dataPtr)
			state.decl(tempSymbol)
			state.analyzeNode(tempWrite)
			access.symbol = tempSymbol
			access.deref = 1
			access.type = access.symbol.type.typeAfterDeref()
			access.isFieldAccess = True
		elif isVec:
			dataPtr = access.moveToClone()
			dataPtr.field = dataPtr.type.fields[2]
			dataPtr.fieldSpan = dataPtr.span
			dataPtr.staticOffset += dataPtr.field.offset
			dataPtr.type = dataPtr.type.fields[2].type.baseType
			dataPtr.borrow = True
			dataPtr.borrows = {dataPtr}
			
			(tempSymbol, tempWrite) = createTempSymbol(dataPtr)
			state.decl(tempSymbol)
			state.analyzeNode(tempWrite)
			access.symbol = tempSymbol
			access.deref = 1
			access.type = access.symbol.type.typeAfterDeref()
			access.isFieldAccess = True
		elif not access.type.isArrayType:
			failed = True
			logError(state, expr.expr.span, 
				'base of index expression must be an array type (found {})'.format(access.type))
		elif index.type and index.type != USize:
			failed = True
			logError(state, expr.index.span, 'index must be type usize (found {})'.format(index.type))
		else:
			count = access.type.count
			fields = access.type.fields
			access.type = access.type.baseType
		
		if failed:
			access.type = None
			access.typeFailed = True
			return
		
		staticIndex = index.staticEval(state)
		if None not in (staticIndex, count, fields):
			assert staticIndex.dataType == StaticDataType.INT
			if staticIndex.data >= count:
				logError(state, expr.index.span, 
					'index {} out of range for array of size {}'.format(staticIndex.data, count))
			else:
				fieldInfo = fields[staticIndex.data]
				access.staticOffset += fieldInfo.offset
				if len(access.dynOffsets) == 0:
					access.field = fieldInfo
					access.fieldSpan = index.span
		else:
			access.dynOffsets.append(DynOffset(index, access.type))
	else:
		(symbol, write) = createTempSymbol(expr)
		access.symbol = symbol
		state.decl(symbol)
		state.analyzeNode(write)
		access.type = access.symbol.type
