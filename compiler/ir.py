from io                       import StringIO
from .ast                     import FnDeclAST, FnCallAST, ValueRefAST, StrLitAST, BlockAST, \
                                     IntLitAST, ReturnAST, LetAST, IfAST, InfixOpAST, InfixOp, \
                                     CoercionAST, BoolLitAST, WhileAST, AsgnAST, DerefAST, TupleLitAST, \
                                     IndexOpAST, VoidAST, AddressAST, FloatLitAST, BreakAST, ArrayLitAST, \
                                     ContinueAST, LoopAST, CharLitAST, StructLitAST, FieldAccessAST, \
                                     ValueExprAST, FnParamAST, ModLevelDeclAST, SignAST, CMP_OPS
from .                        import types

# def structBytes(structLit):
# 	bytes = []
# 	for field in structLit.resolvedType.fields:

class FundamentalType:
	def __init__(self, byteSize, isFloatType=False, isCompositeType=False):
		self.byteSize = byteSize
		self.isFloatType = isFloatType
		self.isCompositeType = isCompositeType
	
	def __str__(self):
		return '{}{}'.format(
			'f' if self.isFloatType else 's' if self.isCompositeType else 'i', 
			self.byteSize * 8)
	
	def __repr__(self):
		return str(self)
	
	@staticmethod
	def fromResolvedType(resolvedType):
		if resolvedType.isFloatType:
			if resolvedType.byteSize == 4:
				return F32
			elif resolvedType.byteSize == 8:
				return F64
		elif resolvedType.byteSize <= 8:
			if resolvedType.byteSize == 1:
				return I8
			elif resolvedType.byteSize == 2:
				return I16
			elif resolvedType.byteSize == 4:
				return I32
			elif resolvedType.byteSize == 8:
				return I64
		
		return FundamentalType(resolvedType.byteSize, isCompositeType=True)

I8   = FundamentalType(1)
I16  = FundamentalType(2)
I32  = FundamentalType(4)
I64  = FundamentalType(8)
F32  = FundamentalType(4, isFloatType=True)
F64  = FundamentalType(8, isFloatType=True)

IPTR = I64 # platform dependent

class Instr:
	def __init__(self, ast):
		self.ast = ast
		
	def affectStack(self, state):
		pass
	
	def pretty(self, fnIR):
		return self.__str__()
	
	def __str__(self):
		assert 0
	
	def __repr__(self):
		return self.__str__()

class Imm(Instr):
	def __init__(self, ast, type, value):
		super().__init__(ast)
		self.type = type
		self.value = value
		
	def affectStack(self, state):
		state.pushOperand(self.type)
	
	def __str__(self):
		return 'imm {}.{}'.format(self.value, self.type)

class Struct(Instr):
	def __init__(self, ast, fType):
		self.ast = ast
		self.type = fType
		
	def affectStack(self, state):
		state.pushOperand(self.type)
	
	def __str__(self):
		return 'struct {}'.format(self.type)

class Raise(Instr):
	def __init__(self, ast, offset):
		super().__init__(ast)
		self.offset = offset
		assert offset > 0
		
	def affectStack(self, state):
		state.raiseOperand(self.offset)
	
	def __str__(self):
		return 'raise {}'.format(self.offset)	

class Dup(Instr):
	def __init__(self, ast, offset):
		super().__init__(ast)
		self.offset = offset
		
	def affectStack(self, state):
		state.dupOperand(self.offset)
	
	def __str__(self):
		return 'dup {}'.format(self.offset)

class Swap(Instr):
	def __init__(self, ast, offset):
		super().__init__(ast)
		self.offset = offset
		assert offset > 0
		
	def affectStack(self, state):
		state.swapOperand(self.offset)
	
	def __str__(self):
		return 'swap {}'.format(self.offset)

class Pop(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
	
	def __str__(self):
		return 'pop'

class Static(Instr):
	def __init__(self, ast, type, label):
		super().__init__(ast)
		self.type = type
		self.label = label
		
	def affectStack(self, state):
		state.pushOperand(self.type)
	
	def __str__(self):
		return 'static {}.{}'.format(self.label, self.type)

class Global(Instr):
	def __init__(self, ast, type, label):
		super().__init__(ast)
		self.type = type
		self.label = label
		
	def affectStack(self, state):
		state.pushOperand(self.type)
	
	def __str__(self):
		return 'global {}.{}'.format(self.label, self.type)

class Neg(Instr):
	def __init__(self, ast):
		super().__init__(ast)
	
	def __str__(self):
		return 'neg'

class FNeg(Instr):
	def __init__(self, ast):
		super().__init__(ast)
	
	def __str__(self):
		return 'fneg'

class IExtend(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == False
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'iextend {}'.format(self.type)

class Extend(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == False
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'extend {}'.format(self.type)

class Truncate(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == False
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'truncate {}'.format(self.type)

class FExtend(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == True
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'fextend {}'.format(self.type)

class FTruncate(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == True
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'ftruncate {}'.format(self.type)

class IToF(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == True
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'itof {}'.format(self.type)

class UToF(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == True
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'utof {}'.format(self.type)

class FToI(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == False
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'ftoi {}'.format(self.type)

class FToU(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		assert type.isFloatType == False
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'ftou {}'.format(self.type)

class Addr(Instr):
	def __init__(self, ast, offset):
		self.ast = ast
		self.offset = offset
	
	def affectStack(self, state):
		state.pushOperand(IPTR)
	
	def __str__(self):
		return 'addr {}'.format(self.offset)

class Call(Instr):
	def __init__(self, ast, argCt, retTypes, cVarArgs):
		super().__init__(ast)
		self.retTypes = retTypes
		self.argCt = argCt
		self.cVarArgs = cVarArgs
		
	def affectStack(self, state):
		for _ in range(0, self.argCt + 1):
			state.popOperand()
		
		for t in self.retTypes:
			state.pushOperand(t)
	
	def __str__(self):
		retTypes = ', '.join([str(t) for t in self.retTypes])
		cVarArgs = '...' if self.cVarArgs else ''
		return 'call({}{}) -> ({})'.format(self.argCt, cVarArgs, retTypes)

class Deref(Instr):
	def __init__(self, ast, type):
		super().__init__(ast)
		self.type = type
		
	def affectStack(self, state):
		state.replaceTopOperand(self.type)
	
	def __str__(self):
		return 'deref -> {}'.format(self.type)

class DerefW(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.popOperand()
	
	def __str__(self):
		return 'derefw'

class Field(Instr):
	def __init__(self, ast, offset, fType):
		super().__init__(ast)
		self.offset = offset
		self.type = fType
	
	def affectStack(self, state):
		state.popOperand()
		state.pushOperand(self.type)
	
	def __str__(self):
		return 'field {} -> {}'.format(self.offset, self.type)

class DerefField(Instr):
	def __init__(self, ast, offset, fType):
		super().__init__(ast)
		self.offset = offset
		self.type = fType
	
	def affectStack(self, state):
		state.popOperand()
		state.pushOperand(self.type)
	
	def __str__(self):
		return 'deref_field {} -> {}'.format(self.offset, self.type)

class FieldW(Instr):
	def __init__(self, ast, offset):
		super().__init__(ast)
		self.offset = offset
	
	def affectStack(self, state):
		state.popOperand()
		state.popOperand()
	
	def __str__(self):
		return 'fieldw {}'.format(self.offset)

class DerefFieldW(Instr):
	def __init__(self, ast, offset):
		super().__init__(ast)
		self.offset = offset
	
	def affectStack(self, state):
		state.popOperand()
		state.popOperand()
	
	def __str__(self):
		return 'deref_fieldw {}'.format(self.offset)

class Ret(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def __str__(self):
		return 'ret'

class BlockMarker(Instr):
	def __init__(self, ast, index):
		super().__init__(ast)
		self.index = index
	
	def pretty(self, fnIR):
		inputs = ', '.join([str(t) for t in fnIR.blockDefs[self.index].inputs])
		return '.{}({}):'.format(fnIR.blockDefs[self.index].label, inputs)
	
	def __str__(self):
		return '.<{}>:'.format(self.index)

class Br(Instr):
	def __init__(self, ast, index):
		super().__init__(ast)
		self.index = index
	
	def pretty(self, fnIR):
		return 'br .{}'.format(fnIR.blockDefs[self.index].label)
	
	def __str__(self):
		return 'br .<{}>'.format(self.index)

class BrIf(Instr):
	def __init__(self, ast, ifIndex, elseIndex):
		super().__init__(ast)
		self.index = ifIndex
		self.elseIndex = elseIndex
		
	def affectStack(self, state):
		state.popOperand()
	
	def pretty(self, fnIR):
		return 'br_if .{}, .{}'.format(
			fnIR.blockDefs[self.index].label, fnIR.blockDefs[self.elseIndex].label)
	
	def __str__(self):
		return 'br_if .<{}>, .<{}>'.format(self.index, self.elseIndex)

class Eq(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.replaceTopOperand(I8)
		
	def __str__(self):
		return 'eq'

class NEq(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.replaceTopOperand(I8)
	
	def __str__(self):
		return 'neq'

class Less(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.replaceTopOperand(I8)
	
	def __str__(self):
		return 'less'

class LessEq(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.replaceTopOperand(I8)
	
	def __str__(self):
		return 'lesseq'

class Greater(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.replaceTopOperand(I8)
	
	def __str__(self):
		return 'greater'

class GreaterEq(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
		state.replaceTopOperand(I8)
	
	def __str__(self):
		return 'greatereq'

class Add(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
	
	def __str__(self):
		return 'add'

class Sub(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
	
	def __str__(self):
		return 'sub'

class Mul(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
	
	def __str__(self):
		return 'mul'

class Div(Instr):
	def __init__(self, ast):
		super().__init__(ast)
		
	def affectStack(self, state):
		state.popOperand()
	
	def __str__(self):
		return 'div'

def letToIR(state, ast):
	if ast.resolvedSymbolType.isVoidType:
		if ast.expr:
			exprToIR(state, ast.expr)
		return
	
	if ast.expr:
		exprToIR(state, ast.expr)
		if ast.unused:
			state.appendInstr(Pop(ast))
		else:
			state.nameTopOperand(ast)
	# else:
	# 	fType = FundamentalType.fromResolvedType(ast.resolvedSymbolType)
	# 	state.appendInstr(Imm(ast, fType, 0))
	# 	state.nameTopOperand(ast)

def derefToIR(state, ast):
	exprToIR(state, ast.expr)
	for i in range(0, ast.derefCount):
		if i+1 < ast.derefCount:
			fType = IPTR
		else:
			fType = FundamentalType.fromResolvedType(ast.resolvedType)
		
		state.appendInstr(Deref(ast, fType))

def signToIR(state, ast):
	exprToIR(state, ast.expr)
	if ast.negate:
		if ast.resolvedType.isFloatType:
			state.appendInstr(FNeg(ast))
		else:
			state.appendInstr(Neg(ast))

def asgnToIR(state, ast):
	if ast.lvalue.resolvedType.isVoidType:
		exprToIR(state, ast.rvalue)
		return
	
	if type(ast.lvalue) == DerefAST:
		exprToIR(state, ast.lvalue.expr)
		for _ in range(0, ast.lvalue.derefCount-1):
			state.appendInstr(Deref(ast.lvalue, IPTR))
		exprToIR(state, ast.rvalue)
		state.appendInstr(DerefW(ast))
	elif type(ast.lvalue) in (FieldAccessAST, IndexOpAST):
		swap = False
		deref = False
		expr = ast.lvalue.expr
		if type(expr) == DerefAST:
			deref = True
			expr = expr.expr
		
		if type(expr) != ValueRefAST:
			swap = True
			exprToIR(state, ast.lvalue)
		
		exprToIR(state, ast.rvalue)
		
		if type(ast.lvalue) == FieldAccessAST:
			state.appendInstr(Imm(ast, IPTR, ast.lvalue.fieldOffset))
		else:
			exprToIR(state, ast.lvalue.index)
		
		if type(expr) == ValueRefAST:
			stackOffset = state.localOffset(expr.symbol)
		else:
			stackOffset = 2
		
		if deref:
			state.appendInstr(DerefFieldW(ast, stackOffset))
		else:
			state.appendInstr(FieldW(ast, stackOffset))
		
		if swap:
			state.appendInstr(Swap(ast, stackOffset))
	elif ast.lvalue.symbol not in state.operandsBySymbol:
		state.nameTopOperand(ast.lvalue.symbol)
	else:
		state.appendInstr(Swap(ast, state.localOffset(ast.lvalue.symbol)))

def indexToIR(state, ast):
	deref = False
	swap = False
	if type(ast.expr) == DerefAST:
		deref = True
		exprToIR(state, ast.expr.expr)
		for _ in range(0, ast.expr.derefCount-1):
			state.appendInstr(Deref(ast.expr, IPTR))
		stackOffset = 1
		swap = True
	elif type(ast.expr) == ValueRefAST:
		stackOffset = state.localOffset(ast.expr.symbol) + 1
	else:
		exprToIR(state, ast.expr)
		stackOffset = 1
		swap = True
	
	exprToIR(state, ast.index)
	mul = types.getAlignedSize(ast.expr.resolvedType.baseType)
	if mul > 1:
		state.appendInstr(Imm(ast, IPTR, mul))
		state.appendInstr(Mul(ast))
	
	fType = FundamentalType.fromResolvedType(ast.resolvedType)
	if deref:
		state.appendInstr(DerefField(ast, stackOffset, fType))
	else:
		state.appendInstr(Field(ast, stackOffset, fType))
	
	if swap:
		state.appendInstr(Swap(ast, stackOffset))

def boolLitToIR(state, ast):
	state.appendInstr(Imm(ast, I8, 1 if ast.value else 0))

def charLitToIR(state, ast):
	state.appendInstr(Imm(ast, I32, ast.value))

def intLitToIR(state, ast):
	fType = FundamentalType.fromResolvedType(ast.resolvedType)
	state.appendInstr(Imm(ast, fType, ast.value))

# def floatLitToIR(scope, lit, fn, block):
# 	state.regCt += 1
	
# 	type = FundamentalType.fromResolvedType(lit.resolvedType)
# 	src = Target(Storage.IMM, type, lit.value)
# 	dest = Target(Storage.LOCAL, type, state.regCt)
	
# 	block.append(Move(lit, src, dest, type))

def initCompositeFields(state, lit, baseOffset):
	if type(lit) == StructLitAST:
		initStructFields(state, lit, baseOffset)
		return
	
	for (init, fieldInfo) in zip(lit.values, lit.resolvedType.fields):
		if init.resolvedType.isCompositeType:
			initCompositeFields(state, init, baseOffset + fieldInfo.offset)
			continue
		
		exprToIR(state, init)
		state.appendInstr(Imm(init, IPTR, baseOffset + fieldInfo.offset))
		state.appendInstr(FieldW(init, 2))

def initStructFields(state, structLit, baseOffset):
	fieldDict = structLit.resolvedType.fieldDict
	for init in structLit.fields:
		fieldInfo = fieldDict[init.name]
		if init.expr.resolvedType.isCompositeType:
			initCompositeFields(init.expr, baseOffset + fieldInfo.offset)
			continue
		
		exprToIR(state, init.expr)
		state.appendInstr(Imm(init, IPTR, baseOffset + fieldInfo.offset))
		state.appendInstr(FieldW(init, 2))

def structLitToIR(state, ast):
	fType = FundamentalType.fromResolvedType(ast.resolvedType)
	state.appendInstr(Struct(ast, fType))
	initStructFields(state, ast, 0)

def arrayLitToIR(state, ast):
	fType = FundamentalType.fromResolvedType(ast.resolvedType)
	state.appendInstr(Struct(ast, fType))
	initCompositeFields(state, ast, 0)

def tupleLitToIR(state, ast):
	fType = FundamentalType.fromResolvedType(ast.resolvedType)
	state.appendInstr(Struct(ast, fType))
	initCompositeFields(state, ast, 0)

def strLitToIR(state, ast):
	label = '{}__static__{}'.format(state.name, len(state.staticDefs))
	state.staticDefs.append(StaticDef(label, ast.value))
	state.appendInstr(Static(ast, IPTR, label))

def fieldAccessToIR(state, ast):
	state.appendInstr(Imm(ast, IPTR, ast.fieldOffset))
	
	swap = False
	if type(ast.expr) == DerefAST:
		deref = True
		exprToIR(state, ast.expr.expr)
		for _ in range(0, ast.expr.derefCount-1):
			state.appendInstr(Deref(ast.expr, IPTR))
		stackOffset = 1
		swap = True
	elif type(ast.expr) == ValueRefAST:
		stackOffset = state.localOffset(ast.expr.symbol)
	else:
		exprToIR(state, ast.expr)
		stackOffset = 1
		swap = True
	
	fType = FundamentalType.fromResolvedType(ast.resolvedType)
	state.appendInstr(Field(ast, stackOffset, fType))
	
	if swap:
		state.appendInstr(Swap(ast, stackOffset))

def fnCallToIR(state, ast):
	normalArgs = ast.args
	cVarArgs = []
	if ast.expr.resolvedType.cVarArgs:
		numParams = len(ast.expr.resolvedType.resolvedParamTypes)
		normalArgs = ast.args[:numParams]
		cVarArgs = ast.args[numParams:]
	
	for expr in normalArgs:
		exprToIR(state, expr)
	
	for expr in cVarArgs:
		exprToIR(state, expr)
		if expr.resolvedType.isVoidType:
			continue
		
		fType = FundamentalType.fromResolvedType(expr.resolvedType)
		if fType.isFloatType:
			assert 0
		elif fType.byteSize < 4:
			if expr.resolvedType.isSigned:
				state.appendInstr(IExtend(ast, IPTR))
			else:
				state.appendInstr(Extend(ast, IPTR))
	
	exprToIR(state, ast.expr)
	if ast.resolvedType.isVoidType:
		retTypes = []
	else:
		retTypes = [FundamentalType.fromResolvedType(ast.resolvedType)]
	state.appendInstr(Call(ast, len(ast.args), retTypes, ast.expr.resolvedType.cVarArgs))

def valueRefToIR(state, ast):
	if isinstance(ast.symbol, ModLevelDeclAST):
		fType = FundamentalType.fromResolvedType(ast.resolvedType)
		state.appendInstr(Global(ast, fType, ast.symbol.mangledName))
	else:
		offset = state.localOffset(ast.symbol)
		
		if ast.lastUse and not state.localIsLoopInput(ast.symbol):
			if offset > 0:
				state.appendInstr(Raise(ast, offset))
			
			if ast.resultUnused:
				state.appendInstr(Pop(ast))
			else:
				state.nameTopOperand(None)
		else:
			if ast.lastUse:
				assert ast.name not in state.loopInfo.lastUses
				state.loopInfo.lastUses.add(ast.symbol)
			
			state.appendInstr(Dup(ast, offset))

def addToIR(state, ast):
	exprToIR(state, ast.l)
	
	if ast.l.resolvedType.isPtrType:
		exprToIR(state, ast.r)
		if ast.l.resolvedType.indirectionLevel == 1:
			mul = ast.l.resolvedType.baseType.byteSize
		else:
			mul = IPTR.byteSize
		state.appendInstr(Imm(ast, IPTR, mul))
		state.appendInstr(Mul(ast))
		state.appendInstr(Add(ast))
	elif ast.r.resolvedType.isPtrType:
		if ast.r.resolvedType.indirectionLevel == 1:
			mul = ast.r.resolvedType.baseType.byteSize
		else:
			mul = IPTR.byteSize
		state.appendInstr(Imm(ast, IPTR, mul))
		state.appendInstr(Mul(ast))
		exprToIR(state, ast.r)
		state.appendInstr(Add(ast))
	else:
		exprToIR(state, ast.r)
		state.appendInstr(Add(ast))

def subToIR(state, ast):
	exprToIR(state, ast.l)
	
	if ast.l.resolvedType.isPtrType:
		exprToIR(state, ast.r)
		if ast.l.resolvedType.indirectionLevel == 1:
			mul = ast.l.resolvedType.baseType.byteSize
		else:
			mul = IPTR.byteSize
		state.appendInstr(Imm(ast, IPTR, mul))
		state.appendInstr(Mul(ast))
		state.appendInstr(Sub(ast))
	else:
		exprToIR(state, ast.r)
		state.appendInstr(Sub(ast))

def mulToIR(state, ast):
	exprToIR(state, ast.l)
	exprToIR(state, ast.r)
	state.appendInstr(Mul(ast))

def divToIR(scope, ast):
	exprToIR(state, ast.l)
	exprToIR(state, ast.r)
	state.appendInstr(Div(ast))

def cmpToIR(state, ast):
	exprToIR(state, ast.l)
	exprToIR(state, ast.r)
	
	if ast.op == InfixOp.EQ:
		instr = Eq(ast)
	elif ast.op == InfixOp.NEQ:
		instr = NEq(ast)
	elif ast.op == InfixOp.GREATER:
		instr = Greater(ast)
	elif ast.op == InfixOp.LESS:
		instr = Less(ast)
	elif ast.op == InfixOp.GREATEREQ:
		instr = GreaterEq(ast)
	elif ast.op == InfixOp.LESSEQ:
		instr = LessEq(ast)
	else:
		assert 0
	
	state.appendInstr(instr)

def coercionToIR(state, ast):
	exprToIR(state, ast.expr)
	
	if ast.typeRef.resolvedType.isVoidType:
		return
	
	fromType = FundamentalType.fromResolvedType(ast.expr.resolvedType)
	toType = FundamentalType.fromResolvedType(ast.typeRef.resolvedType)
	
	fromSigned = ast.expr.resolvedType.isSigned
	toSigned = ast.typeRef.resolvedType.isSigned
	
	if fromType.byteSize == toType.byteSize and fromType.isFloatType == toType.isFloatType:
		return
	
	instr = None
	if fromType.isFloatType == False and toType.isFloatType == False:
		if fromType.byteSize < toType.byteSize:
			if toSigned:
				instr = IExtend(ast, toType)
			else:
				instr = Extend(ast, toType)
		else:
			instr = Truncate(ast, toType)
	elif fromType.isFloatType == True and toType.isFloatType == True:
		if fromType.byteSize < toType.byteSize:
			instr = FExtend(ast, toType)
		else:
			instr = FTruncate(ast, toType)
	elif fromType.isFloatType == False:
		if fromSigned:
			instr = IToF(ast, toType)
		else:
			instr = UToF(ast, toType)
	else:
		if toSigned:
			instr = FToI(ast, toType)
		else:
			instr = FToU(ast, toType)
	
	state.appendInstr(instr)

def getInputInfo(state):
	inputTypes = []
	inputSymbols = []
	for localInfo in state.operandStack:
		inputTypes.append(localInfo.type)
		inputSymbols.append(localInfo.symbol)
	
	return inputTypes, inputSymbols

def beginBlock(state, ast, blockDef):
	inputTypes, inputSymbols = getInputInfo(state)
	
	assert len(inputTypes) == len(blockDef.inputs)
	for t, u in zip(inputTypes, blockDef.inputs):
		assert t == u
	
	state.setupLocals(inputTypes, inputSymbols)
	state.appendInstr(BlockMarker(ast, blockDef.index))

def loopToIR(state, ast, hasTest=False):
	inputTypes, inputSymbols = getInputInfo(state)
	
	continueBlock = state.defBlock(inputTypes, True)
	if hasTest:
		ifBlock = state.defBlock(inputTypes)
	breakBlock = state.defBlock(inputTypes)
	
	state.appendInstr(Br(ast, continueBlock.index))
	beginBlock(state, ast, continueBlock)
	
	state.pushLoopInfo(ast, continueBlock, breakBlock, inputSymbols)
	
	if hasTest:
		exprToIR(state, ast.expr)
		state.appendInstr(BrIf(ast, ifBlock.index, breakBlock.index))
		state.appendInstr(BlockMarker(ast, ifBlock.index))
	
	blockToIR(state, ast.block)
	lastType = type(state.instr[-1])
	if lastType not in (Br, BrIf, Ret):
		state.appendInstr(Br(ast, continueBlock.index))
	
	state.setupLocals(inputTypes, inputSymbols)
	state.appendInstr(BlockMarker(ast, breakBlock.index))
	for symbol in state.loopInfo.lastUses:
		if len(state.loopInfoStack) > 1 and symbol in state.loopInfoStack[-2].inputSymbols:
			state.loopInfoStack[-2].lastUses.add(symbol)
		else:
			dropSymbol(state, symbol)
	
	state.popLoopInfo()

def whileToIR(state, ast):
	loopToIR(state, ast, True)

def ifToIR(state, ast):
	exprToIR(state, ast.expr)
	
	inputTypes, inputSymbols = getInputInfo(state)
	inputTypes, inputSymbols = inputTypes[:-1], inputSymbols[:-1]
	
	ifBlock = state.defBlock(inputTypes)
	elseBlock = state.defBlock(inputTypes)
	
	state.appendInstr(BrIf(ast, ifBlock.index, elseBlock.index))
	
	beginBlock(state, ast.block, ifBlock)
	blockToIR(state, ast.block)
	
	endIfBlock = None
	lastType = type(state.instr[-1])
	if lastType not in (Br, BrIf, Ret):
		endInputTypes, endInputNames = getInputInfo(state)
		endIfBlock = state.defBlock(endInputTypes)
		state.appendInstr(Br(ast, endIfBlock.index))
	
	state.setupLocals(inputTypes, inputSymbols)
	state.appendInstr(BlockMarker(ast.elseBlock, elseBlock.index))
	blockToIR(state, ast.elseBlock)
	
	lastType = type(state.instr[-1])
	if lastType not in (Br, BrIf, Ret):
		if endIfBlock == None:
			endInputTypes, endInputNames = getInputInfo(state)
			endIfBlock = state.defBlock(endInputTypes)
		state.appendInstr(Br(ast, endIfBlock.index))
	
	if endIfBlock != None:
		assert not ast.doesReturn and not ast.doesBreak
		state.setupLocals(endInputTypes, endInputNames)
		beginBlock(state, ast, endIfBlock)

def breakOrContinueToIR(state, ast, isBreak):
	blockDef = state.loopInfo.breakBlock if isBreak else state.loopInfo.continueBlock
	
	for symbol in ast.dropSymbols:
		if symbol not in state.operandsBySymbol:
			continue
		dropSymbol(state, symbol)
	
	state.appendInstr(Br(ast, blockDef.index))

def breakToIR(state, ast):
	breakOrContinueToIR(state, ast, True)

def continueToIR(state, ast):
	breakOrContinueToIR(state, ast, False)

def returnToIR(state, ast):
	if ast.expr != None:
		exprToIR(state, ast.expr)
	
	state.appendInstr(Ret(ast))

def addressToIR(state, ast):
	offset = None
	if type(ast.expr) != ValueRefAST or isinstance(ast.expr.symbol, ModLevelDeclAST):
		exprToIR(state, ast.expr)
		offset = 0
	else:
		offset = state.localOffset(ast.expr.symbol)
	
	state.appendInstr(Addr(ast, offset))

def exprToIR(state, expr):
	if type(expr) == VoidAST:
		pass
	elif type(expr) == BoolLitAST:
		boolLitToIR(state, expr)
	elif type(expr) == IntLitAST:
		intLitToIR(state, expr)
	# elif type(expr) == FloatLitAST:
	# 	floatLitToIR(state, expr)
	elif type(expr) == StrLitAST:
		strLitToIR(state, expr)
	elif type(expr) == CharLitAST:
		charLitToIR(state, expr)
	elif type(expr) == StructLitAST:
		structLitToIR(state, expr)
	elif type(expr) == TupleLitAST:
		tupleLitToIR(state, expr)
	elif type(expr) == ArrayLitAST:
		arrayLitToIR(state, expr)
	elif type(expr) == ValueRefAST:
		valueRefToIR(state, expr)
	elif type(expr) == FieldAccessAST:
		fieldAccessToIR(state, expr)
	elif type(expr) == SignAST:
		signToIR(state, expr)
	elif type(expr) == DerefAST:
		derefToIR(state, expr)
	elif type(expr) == AddressAST:
		addressToIR(state, expr)
	elif type(expr) == FnCallAST:
		fnCallToIR(state, expr)
	elif type(expr) == LetAST:
		letToIR(state, expr)
	elif type(expr) == ReturnAST:
		returnToIR(state, expr)
	elif type(expr) == BreakAST:
		breakToIR(state, expr)
	elif type(expr) == ContinueAST:
		continueToIR(state, expr)
	elif type(expr) == CoercionAST:
		coercionToIR(state, expr)
	elif type(expr) == IndexOpAST:
		indexToIR(state, expr)
	elif type(expr) == InfixOpAST:
		if expr.op == InfixOp.PLUS:
			addToIR(state, expr)
		elif expr.op == InfixOp.MINUS:
			subToIR(state, expr)
		elif expr.op == InfixOp.TIMES:
			mulToIR(state, expr)
		elif expr.op == InfixOp.DIV:
			divToIR(state, expr)
		elif expr.op in CMP_OPS:
			cmpToIR(state, expr)
		else:
			assert 0
	elif type(expr) == AsgnAST:
		asgnToIR(state, expr)
	elif type(expr) == BlockAST:
		blockToIR(state, expr)
	elif type(expr) == IfAST:
		ifToIR(state, expr)
	elif type(expr) == LoopAST:
		loopToIR(state, expr)
	elif type(expr) == WhileAST:
		whileToIR(state, expr)
	else:
		assert 0
	
	if isinstance(expr, ValueExprAST) and expr.resultUnused:
		if expr.resolvedType.isVoidType:
			pass
		elif type(expr) in (ValueRefAST, IfAST, BlockAST):
			pass
		else:
			state.appendInstr(Pop(expr))

def dropSymbol(state, symbol):
	offset = state.localOffset(symbol)
	if offset > 0:
		state.appendInstr(Raise(symbol, offset))
	state.appendInstr(Pop(symbol))

def blockToIR(state, block):
	for symbol in block.dropSymbols:
		dropSymbol(state, symbol)
	
	for expr in block.exprs:
		exprToIR(state, expr)
		if type(expr) in (BreakAST, ContinueAST, ReturnAST):
			break

class StaticDef:
	def __init__(self, label, value):
		self.label = label
		self.value = value

class BlockDef:
	def __init__(self, index, label, inputs, hasBackwardsCallers):
		self.index = index
		self.label = label
		self.inputs = inputs
		self.hasBackwardsCallers = hasBackwardsCallers

class OperandInfo:
	def __init__(self, index, fType, symbol=None):
		self.index = index
		self.type = fType
		self.symbol = symbol

class LoopInfo:
	def __init__(self, ast, continueBlock, breakBlock, inputSymbols):
		self.ast = ast
		self.continueBlock = continueBlock
		self.breakBlock = breakBlock
		self.lastUses = set()
		self.inputSymbols = inputSymbols

class IRState:
	def __init__(self, fnDecl):
		self.ast = fnDecl
		self.name = fnDecl.mangledName
		self.cVarArgs = fnDecl.cVarArgs
		self.instr = []
		self.loopInfoStack = []
		self.loopInfo = None
		self.staticDefs = []
		self.retTypes = []
		self.paramTypes = []
		self.blockDefs = []
		self.operandStack = []
		self.operandsBySymbol = {}
		
		inputSymbols = []
		for param in fnDecl.params:
			if param.resolvedSymbolType.isVoidType:
				continue
			
			fType = FundamentalType.fromResolvedType(param.resolvedSymbolType)
			inputSymbols.append(param)
			self.paramTypes.append(fType)
		
		self.setupLocals(self.paramTypes, inputSymbols)
		
		# print('{}({}) -> ({})'.format(self.name,
		# 	', '.join([str(t) for t in self.paramTypes]),
		# 	', '.join([str(t) for t in self.retTypes])))
		
		for param in reversed(fnDecl.params):
			if param.resolvedSymbolType.isVoidType:
				continue
			
			if param.unused:
				offset = self.localOffset(param)
				if offset > 0:
					self.appendInstr(Raise(param, offset))
				self.appendInstr(Pop(param))
		
		if fnDecl.resolvedSymbolType.resolvedReturnType.isVoidType:
			self.retTypes = []
		else:
			fType = FundamentalType.fromResolvedType(fnDecl.resolvedSymbolType.resolvedReturnType)
			self.retTypes = [fType]
	
	def appendInstr(self, instr):
		self.instr.append(instr)
		instr.affectStack(self)
		
		instrText = '{}{}'.format(
			'   ' if type(instr) != BlockMarker else '', instr.pretty(self))
		space = ' ' * (72 - len(instrText))
		# print('{}{}# [{}]'.format(instrText, space, 
		# 	', '.join([(t.symbol.name + ': ' if t.symbol else '') + str(t.type) for t in self.operandStack])))
	
	def defBlock(self, inputs, hasBackwardsCallers=False):
		index = len(self.blockDefs)
		label = '{}__{}'.format(self.name, index)
		blockDef = BlockDef(index, label, inputs, hasBackwardsCallers)
		self.blockDefs.append(blockDef)
		return blockDef
	
	def pushLoopInfo(self, ast, continueBlock, breakBlock, inputSymbols):
		self.loopInfo = LoopInfo(ast, continueBlock, breakBlock, inputSymbols)
		self.loopInfoStack.append(self.loopInfo)
	
	def popLoopInfo(self):
		self.loopInfoStack.pop()
		self.loopInfo = self.loopInfoStack[-1] if len(self.loopInfoStack) > 0 else None
	
	def setupLocals(self, inputTypes, inputSymbols):
		operandStack = []
		operandsBySymbol = {}
		
		for i, t, s in zip(range(0, len(inputTypes)), inputTypes, inputSymbols):
			info = OperandInfo(i, t, s)
			if s != None: operandsBySymbol[s] = info
			operandStack.append(info)
		
		self.operandStack = operandStack
		self.operandsBySymbol = operandsBySymbol
	
	def localIsLoopInput(self, symbol):
		return self.loopInfo and symbol in self.loopInfo.inputSymbols
	
	def localOffset(self, symbol):
		i = len(self.operandStack) - 1 - self.operandsBySymbol[symbol].index
		assert i >= 0
		return i
	
	def nameTopOperand(self, symbol):
		self.operandStack[-1].symbol = symbol
		self.operandsBySymbol[symbol] = self.operandStack[-1]
	
	def pushOperand(self, fType):
		self.operandStack.append(OperandInfo(len(self.operandStack), fType))
	
	def popOperand(self):
		self.removeOperandName(len(self.operandStack) - 1)
		self.operandStack.pop()
	
	def replaceTopOperand(self, fType):
		self.removeOperandName(-1)
		self.operandStack[-1] = OperandInfo(len(self.operandStack) - 1, fType)
	
	def swapOperand(self, offset):
		index = len(self.operandStack) - offset - 1
		symbol = self.operandStack[index].symbol
		self.removeOperandName(index)
		info = self.operandStack.pop()
		info.index = index
		info.symbol = symbol
		if symbol != None:
			self.operandsBySymbol[symbol] = info
		self.operandStack[index] = info
	
	def raiseOperand(self, offset):
		index = len(self.operandStack) - offset - 1
		self.operandStack.append(self.operandStack.pop(index))
		for i in range(index, len(self.operandStack)):
			self.operandStack[i].index = i
	
	def dupOperand(self, offset):
		index = len(self.operandStack) - offset - 1
		self.pushOperand(self.operandStack[index].type)
	
	def removeOperandName(self, index):
		info = self.operandStack[index]
		if info.symbol != None:
			del self.operandsBySymbol[info.symbol]

class FnIR:
	def __init__(self, state):
		self.ast = state.ast
		self.name = state.ast.mangledName
		self.instr = state.instr
		self.staticDefs = state.staticDefs
		self.blockDefs = state.blockDefs
		self.paramTypes = state.paramTypes
		self.retTypes = state.retTypes
	
	def __str__(self):
		s = StringIO()
		s.write('{}({}) -> ({})\n'.format(
			self.name,
			', '.join([str(t) for t in self.paramTypes]),
			', '.join([str(t) for t in self.retTypes])))
		
		for instr in self.instr:
			indent = '' if type(instr) == BlockMarker else '    '
			s.write('{}{}\n'.format(indent, instr.pretty(self), ''))
		
		return s.getvalue()

def fnToIR(fnDecl):
	state = IRState(fnDecl)
	blockToIR(state, fnDecl.body)
	
	lastType = type(state.instr[-1])
	assert lastType != Br and lastType != BrIf
	if lastType != Ret:
		state.appendInstr(Ret(fnDecl))
	
	return FnIR(state)

def generateIR(mod):
	for decl in mod.modDecls:
		generateIR(decl)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		
		decl.ir = fnToIR(decl)