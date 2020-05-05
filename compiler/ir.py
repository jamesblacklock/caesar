import ctypes
from io       import StringIO
from .        import types

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
	def __init__(self, ast, argCt, retType, cVarArgs):
		super().__init__(ast)
		self.retType = retType
		self.argCt = argCt
		self.cVarArgs = cVarArgs
		
	def affectStack(self, state):
		for _ in range(0, self.argCt + 1):
			state.popOperand()
		
		if self.retType:
			state.pushOperand(self.retType)
	
	def __str__(self):
		retType =  ' -> {}'.format(self.retType) if self.retType else ''
		cVarArgs = '...' if self.cVarArgs else ''
		return 'call({}{}){}'.format(self.argCt, cVarArgs, retType)

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
	
	def affectStack(self, state):
		if len(state.operandStack) > 0:
			state.popOperand()
		
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

class StaticDef:
	def __init__(self, label, bytes):
		self.label = label
		self.bytes = bytes

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
	def __init__(self, parent, ast, continueBlock, breakBlock, inputSymbols):
		self.ast = ast
		self.continueBlock = continueBlock
		self.breakBlock = breakBlock
		self.droppedSymbols = set()
		self.inputSymbols = inputSymbols
		self.parent = parent

class IRState:
	def __init__(self, fnDecl):
		self.ast = fnDecl
		self.name = fnDecl.mangledName
		self.cVarArgs = fnDecl.cVarArgs
		self.instr = []
		self.loopInfo = None
		self.staticDefs = []
		self.retType = None
		self.paramTypes = []
		self.blockDefs = []
		self.operandStack = []
		self.operandsBySymbol = {}
		
		inputSymbols = []
		for param in fnDecl.params:
			if param.type.isVoidType:
				continue
			
			fType = FundamentalType.fromResolvedType(param.type)
			inputSymbols.append(param)
			self.paramTypes.append(fType)
		
		self.setupLocals(self.paramTypes, inputSymbols)
		
		print('{}({}){}'.format(self.name,
			', '.join([str(t) for t in self.paramTypes]),
			' -> {}'.format(self.retType) if self.retType else ''))
		
		if not fnDecl.type.returnType.isVoidType:
			self.retType = FundamentalType.fromResolvedType(fnDecl.type.returnType)
	
	def appendInstr(self, instr):
		self.instr.append(instr)
		instr.affectStack(self)
		
		instrText = '{}{}'.format(
			'   ' if type(instr) != BlockMarker else '', instr.pretty(self))
		space = ' ' * (72 - len(instrText))
		print('{}{}# [{}]'.format(instrText, space, 
			', '.join([(t.symbol.name + ': ' if t.symbol else '') + str(t.type) for t in self.operandStack])))
	
	def defBlock(self, inputs, hasBackwardsCallers=False):
		index = len(self.blockDefs)
		label = '{}__{}'.format(self.name, index)
		blockDef = BlockDef(index, label, inputs, hasBackwardsCallers)
		self.blockDefs.append(blockDef)
		return blockDef
	
	def pushLoopInfo(self, ast, continueBlock, breakBlock, inputSymbols):
		self.loopInfo = LoopInfo(self.loopInfo, ast, continueBlock, breakBlock, inputSymbols)
	
	def popLoopInfo(self):
		self.loopInfo = self.loopInfo.parent
	
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
		self.removeOperandName(-1)
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
	
	def initCompositeFields(self, lit, baseOffset):
		if lit.type.isStructType:
			self.initStructFields(lit, baseOffset)
			return
		
		for (init, fieldInfo) in zip(lit.values, lit.type.fields):
			if init.type.isCompositeType:
				self.initCompositeFields(init, baseOffset + fieldInfo.offset)
				continue
			
			init.writeIR(self)
			self.appendInstr(Imm(init, IPTR, baseOffset + fieldInfo.offset))
			self.appendInstr(FieldW(init, 2))

	def initStructFields(self, structLit, baseOffset):
		fieldDict = structLit.type.fieldDict
		for init in structLit.fields:
			fieldInfo = fieldDict[init.name]
			if init.expr.type.isCompositeType:
				self.initCompositeFields(init.expr, baseOffset + fieldInfo.offset)
				continue
			
			init.expr.writeIR(self)
			self.appendInstr(Imm(init, IPTR, baseOffset + fieldInfo.offset))
			self.appendInstr(FieldW(init, 2))

class FnIR:
	def __init__(self, state):
		self.ast = state.ast
		self.name = state.ast.mangledName
		self.instr = state.instr
		self.staticDefs = state.staticDefs
		self.blockDefs = state.blockDefs
		self.paramTypes = state.paramTypes
		self.retType = state.retType
	
	def __str__(self):
		s = StringIO()
		s.write('{}({}){}\n'.format(
			self.name,
			', '.join([str(t) for t in self.paramTypes]),
			' -> {}'.format(self.retType) if self.retType else ''))
		
		for instr in self.instr:
			indent = '' if type(instr) == BlockMarker else '    '
			s.write('{}{}\n'.format(indent, instr.pretty(self), ''))
		
		return s.getvalue()

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

def fnToIR(fnDecl):
	state = IRState(fnDecl)
	fnDecl.body.writeIR(state)
	
	lastType = type(state.instr[-1])
	assert lastType != Br and lastType != BrIf
	if lastType != Ret:
		state.appendInstr(Ret(fnDecl))
	
	return FnIR(state)

def generateIR(mod):
	for decl in mod.mods:
		generateIR(decl)
	
	for decl in mod.fns:
		if decl.extern:
			continue
		
		decl.ir = fnToIR(decl)