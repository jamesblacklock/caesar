import ctypes
from io                                import StringIO
from enum                              import Enum
from .ir                               import Dup, Write, Global, Imm, Static, Deref, Add, Sub, Mul, \
                                              Div, Eq, NEq, Less, LessEq, Greater, GreaterEq, Call, \
                                              Extend, IExtend, Truncate, FExtend, FTruncate, IToF, \
                                              UToF, FToI, FToU, Ret, BrIf, Br, Swap, Pop, Raise, \
                                              Struct, WriteField, ReadField, \
                                              BlockMarker, Neg, Addr, I8, I16, I32, I64, IPTR, F32, F64
from .ir                               import FundamentalType

class Storage(Enum):
	IMM = 'IMM'
	GLOBAL = 'GLOBAL'
	REG = 'REG'
	STACK = 'STACK'
	NONE = 'NONE'

class Usage(Enum):
	SRC = 'SRC'
	DEST = 'DEST'
	ADDR = 'ADDR'
	DEREF = 'DEREF'

def sizeInd(type):
	if type.byteSize == 1:
		return 'byte'
	elif type.byteSize == 2:
		return 'word'
	elif type.byteSize == 4:
		return 'dword'
	else:
		return 'qword'

def floatToHex(value, byteSize):
	if target.type.byteSize == 4:
		return hex(ctypes.c_uint32.from_buffer(ctypes.c_float(value)).value)
	else:
		return hex(ctypes.c_uint64.from_buffer(ctypes.c_double(value)).value)

class Target:
	def __init__(self, fType, storage):
		self.type = fType
		self.storage = storage
		self.active = False
		self.operandIndex = None
	
	def setActive(self, set):
		self.active = set
	
	def render(self, usage, fType, sp):
		assert 0
	
	@staticmethod
	def fromIR(ir):
		if type(ir) == Global or type(ir) == Static:
			return GlobalTarget(ir.type, ir.label)
		elif type(ir) == Struct:
			return Target(ir.type, Storage.NONE)
		elif type(ir) == Imm:
			return ImmTarget(ir.type, ir.value)
		else:
			assert 0

class ImmTarget(Target):
	def __init__(self, fType, value):
		super().__init__(fType, Storage.IMM)
		self.value = value
	
	def render(self, usage, fType, sp):
		assert usage == Usage.SRC
		return '{} {}'.format(sizeInd(fType), self.value)

class GlobalTarget(Target):
	def __init__(self, fType, label):
		super().__init__(fType, Storage.GLOBAL)
		self.label = label
	
	def render(self, usage, fType, sp):
		if usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), self.label)
		elif usage == Usage.ADDR:
			return '[rel {}]'.format(self.label)
		else:
			assert 0

class TopStackTarget(Target):
	def __init__(self, fType, offset):
		super().__init__(fType, Storage.STACK)
		self.offset = offset
	
	def render(self, usage, fType, sp):
		addr = '[rsp + {}]'.format(self.offset)
		if usage == Usage.ADDR:
			return addr
		elif usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), addr)
		else:
			assert 0

class StackTarget(Target):
	def __init__(self, fType, offset):
		super().__init__(fType, Storage.STACK)
		self.offset = offset
	
	def render(self, usage, fType, sp):
		if sp == None:
			addr = '[%% {} %%]'.format(self.offset)
		else:
			addr = '[rsp + {}]'.format(sp - self.offset)
		
		if usage == Usage.ADDR:
			return addr
		elif usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), addr)
		else:
			assert 0

class MultiStackTarget(Target):
	def __init__(self, fType, targets):
		super().__init__(fType, Storage.STACK)
		self.offset = targets[0].offset
		self.targets = targets
	
	def setActive(self, set):
		self.active = set
		for t in self.targets:
			t.setActive(set)
	
	def render(self, usage, fType, sp):
		if sp == None:
			addr = '[%% {} %%]'.format(self.targets[0].offset)
		else:
			addr = '[rsp + {}]'.format((sp - self.targets[0].offset)*8)
		
		if usage == Usage.ADDR:
			return addr
		elif usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), addr)
		else:
			assert 0
	

class RegTarget(Target):
	def __init__(self, calleeSaved, q, d, w, l, h=None):
		super().__init__(None, Storage.REG)
		self.calleeSaved = calleeSaved
		self.l = l; self.h = h
		self.w = w; self.d = d
		self.q = q
	
	def render(self, usage, fType, sp):
		if usage == Usage.DEREF or fType.byteSize == 8:
			s = self.q
		elif fType.byteSize == 4:
			s = self.d
		elif fType.byteSize == 2:
			s = self.w
		elif fType.byteSize == 1:
			s = self.l
		else:
			assert 0
		
		if usage == Usage.DEREF:
			return '{} [{}]'.format(sizeInd(fType), s)
		elif usage in (Usage.SRC, Usage.DEST):
			return s
		else:
			assert 0

class Operand:
	def __init__(self, target, usage, fType=None):
		self.target = target
		self.usage = usage
		self.type = fType if fType else target.type
	
	def render(self, sp):
		return self.target.render(self.usage, self.type, sp)

class Instr:
	def __init__(self, opcode, operands=[], isLabel=False, isComment=False):
		self.opcode = opcode
		self.operands = operands
		self.isLabel = isLabel
		self.isComment = isComment
	
	def render(self, sp):
		assert sp != None
		return self.__str__(sp)
	
	def __str__(self, sp=None):
		ops = (' ' + ', '.join([op.render(sp) for op in self.operands])) if self.operands else ''
		instrText = '{}{}'.format(self.opcode, ops)
		indent = '\t' if self.isLabel else '\t\t'
		prefix = '; ' if self.isComment else ''
		postfix = ':' if self.isLabel and not self.isComment else ''
		return '{}{}{}{}'.format(indent, prefix, instrText, postfix)

class GeneratorState:
	def __init__(self, fnIR):
		self.fnIR = fnIR
		
		self.blockDef = None
		self.blockInputs = [None for _ in fnIR.blockDefs]
		self.instr = []
		self.operandStack = []
		
		self.rax = RegTarget(False, 'rax',  'eax',   'ax',   'al', 'ah')
		self.rbx = RegTarget( True, 'rbx',  'ebx',   'bx',   'bl', 'bh')
		self.rcx = RegTarget(False, 'rcx',  'ecx',   'cx',   'cl', 'ch')
		self.rdx = RegTarget(False, 'rdx',  'edx',   'dx',   'dl', 'dh')
		self.rsi = RegTarget(False, 'rsi',  'esi',   'si',  'sil')
		self.rdi = RegTarget(False, 'rdi',  'edi',   'di',  'dil')
		self.rbp = RegTarget( True, 'rbp',  'ebp',   'bp',  'bpl')
		self.rsp = RegTarget(False, 'rsp',  'esp',   'sp',  'spl')
		self.r8  = RegTarget(False,  'r8',  'r8d',  'r8w',  'r8b')
		self.r9  = RegTarget(False,  'r9',  'r9d',  'r9w',  'r9b')
		self.r10 = RegTarget(False, 'r10', 'r10d', 'r10w', 'r10b')
		self.r11 = RegTarget(False, 'r11', 'r11d', 'r11w', 'r11b')
		self.r12 = RegTarget( True, 'r12', 'r12d', 'r12w', 'r12b')
		self.r13 = RegTarget( True, 'r13', 'r13d', 'r13w', 'r13b')
		self.r14 = RegTarget( True, 'r14', 'r14d', 'r14w', 'r14b')
		self.r15 = RegTarget( True, 'r15', 'r15d', 'r15w', 'r15b')
		self.intRegs = [
			self.rax, self.rbx, self.rbp, self.r10, self.r11, 
			self.r12, self.r13, self.r14, self.r15
		]
		self.intArgRegs = [
			self.rdi, self.rsi, self.rdx, self.rcx, self.r8, 
			self.r9
		]
		self.sp = 0
		self.callStack = []
		self.setupStackFrame()
	
	def allocateStack(self, count=1):
		targets = []
		for _ in range(0, count):
			self.sp += 8
			target = StackTarget(None, self.sp)
			self.callStack.append(target)
			targets.append(target)
		
		return targets
	
	def setupStackFrame(self):
		intRegs = list(reversed(self.intArgRegs))
		floatRegs = []
		stackArgTypes = []
		
		for (i, fType) in enumerate(self.fnIR.paramTypes):
			if fType.isFloatType and len(floatRegs) > 0:
				assert 0
			elif len(intRegs) > 0:
				reg = intRegs.pop()
				reg.type = fType
				self.pushOperand(reg)
				continue
			
			stackArgTypes.append(fType)
		
		if len(stackArgTypes) > 0:
			stackArgTargets = []
			for (i, src) in enumerate(stackArgTypes):
				assert not fType.isFloatType
				target = StackTarget(fType, i - len(stackArgTypes) - 1)
				stackArgTargets.append(target)
				self.callStack.append(target)
			
			for target in reversed(stackArgTargets):
				self.pushOperand(target)
		
		retAddr = StackTarget(I64, -1)
		retAddr.setActive(True)
		self.callStack.append(retAddr)
		
		oldRbp = StackTarget(I64, 0)
		oldRbp.setActive(True)
		self.callStack.append(oldRbp)
		
		self.sp = 0
	
	def appendInstr(self, opcode, *operands, isLabel=False, isComment=False):
		instr = Instr(opcode, operands, isLabel, isComment)
		self.instr.append(instr)
		# print(instr)
	
	def findReg(self, type=None, exclude=[]):
		for reg in self.intRegs:
			if reg.active or reg.calleeSaved or reg in exclude:
				continue
			if type: reg.type = type
			return reg
	
	def findTarget(self, type, alwaysUseStack=False, exclude=[]):
		if type.byteSize > 16:
			alwaysUseStack = True
		
		if not alwaysUseStack:
			target = self.findReg(type, exclude=exclude)
			if target:
				return target
		
		offset = 0
		count = max(1, type.byteSize // 8)
		targets = None
		while offset < len(self.callStack):
			targets = []
			for i in range(0, count):
				t = self.callStack[offset + i] if offset + i >= len(self.callStack) else None
				if not t or t in exclude or t.active:
					offset += i
					targets = None
					break
				
				targets.append(t)
			
			if targets != None:
				break
			
			offset += 1
		
		if targets == None:
			targets = self.allocateStack(count)
		
		for t in targets:
			t.type = type
		
		return targets[0] if count == 1 else MultiStackTarget(type, targets)
	
	def pushIROperand(self, ir):
		target = Target.fromIR(ir)
		target.setActive(True)
		target.operandIndex = len(self.operandStack)
		self.operandStack.append(target)
	
	def clearOperands(self):
		for _ in range(0, len(self.operandStack)):
			self.popOperand()
	
	def pushOperand(self, target):
		assert not target.active
		assert target.operandIndex == None
		assert target.type != None
		
		target.setActive(True)
		target.operandIndex = len(self.operandStack)
		self.operandStack.append(target)
	
	def raiseOperand(self, index):
		opStackLen = len(self.operandStack)
		index = opStackLen - 1 - index
		assert index >= 0
		
		target = self.operandStack.pop(index)
		self.operandStack.append(target)
		
		for i in range(index, len(self.operandStack)):
			self.operandStack[i].operandIndex = i
	
	def swapOperand(self, offset):
		index = len(self.operandStack) - 1 - offset
		assert index >= 0
		
		target = self.operandStack[index]
		target.setActive(False)
		target.operandIndex = None
		target = self.operandStack.pop()
		target.operandIndex = index
		self.operandStack[index] = target
	
	def popOperand(self):
		target = self.operandStack.pop()
		target.setActive(False)
		target.operandIndex = None
		return target
	
	def getOperand(self, offset):
		index = len(self.operandStack) - 1 - offset
		assert index >= 0
		return self.operandStack[index]
	
	def moveOperand(self, src, dest):
		dest.setActive(True)
		dest.operandIndex = src.operandIndex
		dest.type = src.type
		self.operandStack[dest.operandIndex] = dest
		
		src.setActive(False)
		src.operandIndex = None

def irToAsm(state, ir):
	# state.appendInstr(ir.pretty(state.fnIR), isComment=True, isLabel=(type(ir) == BlockMarker))
	
	if type(ir) in (Global, Static, Imm, Struct):
		state.pushIROperand(ir)
	elif type(ir) == Pop:
		state.popOperand()
	elif type(ir) == Dup:
		dup(state, ir)
	elif type(ir) == Swap:
		swap(state, ir)
	elif type(ir) == Raise:
		raise_(state, ir)
	elif type(ir) == Addr:
		addr(state, ir)
	elif type(ir) == Write:
		write(state, ir)
	elif type(ir) == Deref:
		deref(state, ir)
	elif type(ir) == ReadField:
		read_field(state, ir)
	elif type(ir) == WriteField:
		write_field(state, ir)
	elif type(ir) == Extend:
		extend(state, ir)
	elif type(ir) == IExtend:
		iextend(state, ir)
	elif type(ir) == Truncate:
		truncate(state, ir)
	elif type(ir) == Neg:
		neg(state, ir)
	elif type(ir) == Add:
		add(state, ir)
	elif type(ir) == Sub:
		sub(state, ir)
	elif type(ir) == Mul:
		mul(state, ir)
	elif type(ir) == Div:
		div(state, ir)
	elif type(ir) in (Eq, NEq, Less, LessEq, Greater, GreaterEq):
		cmp(state, ir)
	elif type(ir) == Call:
		call(state, ir)
	elif type(ir) == BlockMarker:
		blockMarker(state, ir)
	elif type(ir) == BrIf:
		brIf(state, ir)
	elif type(ir) == Br:
		br(state, ir)
	elif type(ir) == Ret:
		ret(state, ir)
	else:
		assert 0

def moveData(state, src, dest, transferRegister):
	assert src.type.byteSize <= 8
	assert dest.storage in (Storage.STACK, Storage.REG)
	
	if src.storage in (Storage.STACK, Storage.GLOBAL) and dest.storage == Storage.STACK:
		assert transferRegister != None
		state.appendInstr('mov', 
			Operand(transferRegister, Usage.DEST, src.type), 
			Operand(src, Usage.SRC))
		transferRegister.type = src.type
		src = transferRegister
	
	state.appendInstr('mov', 
		Operand(dest, Usage.DEST, src.type), 
		Operand(src, Usage.SRC))

def dup(state, ir):
	src = state.getOperand(ir.offset)
	
	if src.storage == Storage.GLOBAL:
		dest = GlobalTarget(src.type, src.label)
	elif type(ir) == Imm:
		dest = ImmTarget(src.type, src.value)
	else:
		dest = state.findTarget(src.type)
		saveReg(state, state.rax, exclude=[src, dest])
		moveData(state, src, dest, state.rax)
	
	state.pushOperand(dest)

def swap(state, ir):
	state.swapOperand(ir.offset)

def raise_(state, ir):
	state.raiseOperand(ir.offset)

def addr(state, ir):
	src = state.getOperand(ir.offset)
	if src.storage != Storage.STACK:
		newSrc = state.findTarget(src.type, True)
		state.moveOperand(src, newSrc)
		moveData(state, src, newSrc, None)
		src = newSrc
	
	dest = state.findTarget(IPTR)
	state.appendInstr('lea', 
		Operand(dest, Usage.DEST), 
		Operand(src, Usage.ADDR))
	
	state.pushOperand(dest)

def blockMarker(state, ir):
	assert state.blockInputs[ir.index] != None
	state.clearOperands()
	inputTargets = state.blockInputs[ir.index]
	inputTypes = state.fnIR.blockDefs[ir.index].inputs
	
	for target, fType in zip(inputTargets, inputTypes):
		target.type = fType
		state.pushOperand(target)
	
	state.blockDef = state.fnIR.blockDefs[ir.index]
	state.appendInstr(state.blockDef.label, isLabel=True)

def deref(state, ir):
	target = state.getOperand(0)
	
	if target.storage == Storage.REG:
		reg = target
	else:
		reg = state.findReg(target.type)
		if reg == None:
			saveReg(state, state.rax)
			reg = state.rax
			reg.type = ir.type
		
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST, target.type), 
			Operand(target, Usage.SRC))
	
	state.appendInstr('mov', 
		Operand(reg, Usage.DEST, target.type), 
		Operand(reg, Usage.DEREF, target.type))
	
	state.popOperand()
	state.pushOperand(reg)

def write(state, ir):
	src = state.getOperand(0)
	dest = state.getOperand(1)
	
	if dest.storage != Storage.REG:
		reg = state.findReg(I64)
		if reg == None:
			saveReg(state, state.rax, exclude=[src, state.rbp])
			reg = state.rax
			reg.type = I64
		
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST), 
			Operand(dest, Usage.SRC, I64))
		dest = reg
	
	if src.storage != Storage.REG:
		reg = state.findReg(src.type)
		if reg == None:
			saveReg(state, state.rbp, exclude=[dest])
			reg = state.rbp
			reg.type = src.type
		
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST), 
			Operand(src, Usage.SRC))
		src = reg
	
	state.appendInstr('mov', 
		Operand(dest, Usage.DEREF, src.type), 
		Operand(src, Usage.SRC))
	
	state.popOperand()
	state.popOperand()

def moveStruct(state, src, dest, transferRegister):
	sizeRemaining = dest.type.byteSize
	offset = 0
	while sizeRemaining > 0:
		byteSize = 8 if sizeRemaining >= 8 else sizeRemaining
		fType = FundamentalType(byteSize)
		partialSrc = StackTarget(fType, src.offset - offset)
		partialDest = StackTarget(fType, dest.offset - offset)
		moveData(state, partialSrc, partialDest, transferRegister)
		offset += byteSize
		sizeRemaining -= byteSize

def read_field(state, ir):
	reg = state.findReg()
	if reg == None:
		saveReg(state, state.rax)
		reg = state.rax
	
	offsetTarget = state.getOperand(0)
	structTarget = state.getOperand(ir.offset)
	readTarget = state.findTarget(ir.type)
	
	assert offsetTarget.storage == Storage.IMM
	
	fieldTarget = StackTarget(ir.type, structTarget.offset - offsetTarget.value)
	
	if ir.type.byteSize <= 8:
		moveData(state, fieldTarget, readTarget, reg)
	else:
		moveStruct(state, fieldTarget, readTarget, reg)
	
	state.popOperand()
	state.pushOperand(readTarget)

def write_field(state, ir):
	reg = state.findReg()
	if reg == None:
		saveReg(state, state.rax)
		reg = state.rax
	
	offsetTarget = state.getOperand(0)
	valueTarget = state.getOperand(1)
	structTarget = state.getOperand(ir.offset)
	
	if structTarget.storage == Storage.NONE:
		newStructTarget = state.findTarget(structTarget.type, True)
		state.moveOperand(structTarget, newStructTarget)
		structTarget = newStructTarget
	
	assert offsetTarget.storage == Storage.IMM
	
	fieldTarget = StackTarget(valueTarget.type, structTarget.offset - offsetTarget.value)
	
	if valueTarget.type.byteSize <= 8:
		moveData(state, valueTarget, fieldTarget, reg)
	else:
		moveStruct(state, valueTarget, fieldTarget, reg)
	
	state.popOperand()
	state.popOperand()

def extend(state, ir, signed=False):
	src = state.getOperand(0)
	
	if src.storage == Storage.IMM:
		src.type = ir.type
		return
	
	if src.storage == Storage.REG:
		dest = src
	else:
		dest = state.findReg(ir.type)
		if dest == None:
			saveReg(state, state.rax)
			dest = state.rax
	
	opcode = ('movsx' if signed else 'movzx')
	if src.type.byteSize == 4: opcode += 'd'
	
	state.appendInstr(opcode, 
		Operand(dest, Usage.DEST, ir.type), 
		Operand(src, Usage.SRC))
	
	dest.type = ir.type
	
	state.popOperand()
	state.pushOperand(dest)

def iextend(state, ir):
	extend(state, ir, True)

def truncate(state, ir):
	state.getOperand(0).type = ir.type

def neg(state, ir):
	target = state.getOperand(0)
	if target.storage == Storage.IMM:
		target.value = -src.value
	else:
		state.appendInstr('neg', Operand(target, Usage.SRC))

def addOrSub(state, ir, isAdd):
	src = state.getOperand(0)
	dest = state.getOperand(1)
	
	if src.storage == Storage.IMM and dest.storage == Storage.IMM:
		state.popOperand()
		dest.value = (dest.value + src.value) if isAdd else (dest.value - src.value)
		return
	elif dest.storage == Storage.IMM and isAdd:
		dest, src = src, dest
	
	if src.storage == Storage.GLOBAL:
		reg = state.r10 if dest == state.rax else state.rax
		saveReg(state, reg)
		reg.type = src.type
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST), 
			Operand(src, Usage.SRC))
		src = reg
	
	if dest.storage == Storage.GLOBAL:
		reg = state.r10 if src == state.rax else state.rax
		saveReg(state, reg)
		reg.type = dest.type
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST), 
			Operand(dest, Usage.SRC))
		dest = reg
	
	needReg = dest.storage not in (Storage.REG, Storage.STACK) and src.storage not in (Storage.REG, Storage.STACK)
	needReg = needReg or dest.storage == Storage.STACK and src.storage == Storage.STACK
	if needReg:
		saveReg(state, state.rax)
		state.rax.type = dest.type
		state.appendInstr('mov', 
			Operand(state.rax, Usage.DEST), 
			Operand(dest, Usage.SRC))
		dest = state.rax
	
	state.appendInstr('add' if isAdd else 'sub',
			Operand(dest, Usage.DEST), 
			Operand(src, Usage.SRC))
	
	state.popOperand()
	state.popOperand()
	state.pushOperand(dest)

def add(state, ir):
	addOrSub(state, ir, True)

def sub(state, ir):
	addOrSub(state, ir, False)

def mulOrDiv(state, ir, isMul):
	src = state.getOperand(0)
	dest = state.getOperand(1)
	
	if src.storage == Storage.IMM and dest.storage == Storage.IMM:
		state.popOperand()
		dest.value = (dest.value * src.value) if isMul else (dest.value / src.value)
		return
	
	if isMul and src == state.rax:
		dest, src = src, dest
	
	if src != state.rdx:
		saveReg(state, state.rdx)
	
	if dest != state.rax:
		saveReg(state, state.rax)
		state.rax.type = dest.type
		state.appendInstr('mov', 
			Operand(state.rax, Usage.DEST), 
			Operand(dest, Usage.SRC))
		dest = state.rax
		
		src = state.getOperand(0)
	
	if src.storage == Storage.IMM:
		newSrc = state.findTarget(src.type, exclude=[state.rax])
		state.appendInstr('mov', 
			Operand(newSrc, Usage.DEST), 
			Operand(src, Usage.SRC))
		src = newSrc
	
	state.appendInstr('mul' if isMul else 'div', 
		Operand(src, Usage.SRC))
	
	state.popOperand()
	state.popOperand()
	state.pushOperand(dest)

def mul(state, ir):
	mulOrDiv(state, ir, True)

def div(state, ir):
	mulOrDiv(state, ir, False)

def cmp(state, ir):
	saveReg(state, state.rcx)
	state.rcx.type = I8
	
	r = state.getOperand(0)
	l = state.getOperand(1)
	
	if l.storage == Storage.IMM and l.storage == Storage.IMM:
		state.popOperand()
		if type(ir) == Eq:
			l.value = l.value == r.value
		elif type(ir) == NEq:
			l.value = l.value == r.value
		elif type(ir) == Less:
			l.value = l.value == r.value
		elif type(ir) == LessEq:
			l.value = l.value == r.value
		elif type(ir) == Greater:
			l.value = l.value == r.value
		elif type(ir) == GreaterEq:
			l.value = l.value == r.value
		else:
			assert 0
		l.type = I8
		return
	
	if type(ir) == Eq:
		opcode = 'sete'
	elif type(ir) == NEq:
		opcode = 'setne'
	elif type(ir) == Less:
		opcode = 'setl'
	elif type(ir) == LessEq:
		opcode = 'setle'
	elif type(ir) == Greater:
		opcode = 'setg'
	elif type(ir) == GreaterEq:
		opcode = 'setge'
	else:
		assert 0
	
	if l.storage == Storage.STACK and r.storage == Storage.STACK:
		saveReg(state, state.rax)
		state.appendInstr('mov', 
			Operand(state.rax, Usage.DEST, l.type), 
			Operand(l, Usage.SRC))
		l = state.rax
	
	state.appendInstr('xor', 
		Operand(state.rcx, Usage.DEST, I64), 
		Operand(state.rcx, Usage.DEST, I64))
	state.appendInstr('cmp', 
		Operand(l, Usage.DEST), 
		Operand(r, Usage.SRC))
	state.appendInstr(opcode, 
		Operand(state.rcx, Usage.DEST))

	state.popOperand()
	state.popOperand()
	state.pushOperand(state.rcx)

def call(state, ir):
	saveReg(state, state.rax)
	for reg in state.intRegs:
		if reg.calleeSaved:
			continue
		saveReg(state, reg)
	
	funcAddr = state.getOperand(0)
	
	intArgRegs = list(reversed(state.intArgRegs))
	floatRegs = []
	stackArgs = []
	
	for i in reversed(range(1, ir.argCt + 1)):
		src = state.getOperand(i)
		if src.type.isFloatType and len(floatRegs) > 0:
			assert 0
		elif len(intArgRegs) > 0:
			reg = intArgRegs.pop()
			saveReg(state, reg)
			reg.type = src.type
			state.appendInstr('mov', 
				Operand(reg, Usage.DEST), 
				Operand(src, Usage.SRC))
			continue
		
		assert not src.type.isFloatType
		stackArgs.append(src)
	
	if len(stackArgs) > 0:
		requiredStackSpace = len(stackArgs)
		for stackSlot in reversed(state.callStack):
			if stackSlot.active:
				break
			else:
				requiredStackSpace -= 1
		
		state.allocateStack(requiredStackSpace)
		
		for (i, src) in enumerate(stackArgs):
			moveData(state, src, TopStackTarget(src.type, i * 8), state.rax)
	
	if ir.cVarArgs:
		state.appendInstr('xor', 
				Operand(state.rax, Usage.DEST, I8), 
				Operand(state.rax, Usage.DEST, I8))
	
	state.appendInstr('call', Operand(funcAddr, Usage.SRC))
	
	for _ in range(0, ir.argCt + 1):
		state.popOperand()
	
	if len(ir.retTypes) > 0:
		if len(ir.retTypes) > 1:
			assert 0
		
		state.rax.type = ir.retTypes[0]
		state.pushOperand(state.rax)

def brOrBrIf(state, ir, isBrIf):
	offset = 1 if isBrIf else 0
	blockDef = state.fnIR.blockDefs[ir.index]
	inputTypes = blockDef.inputs
	outputTargets = list(reversed([state.getOperand(i + offset) for i in range(0, len(inputTypes))]))
	reg = state.r11
	
	if state.blockInputs[ir.index] == None:
		inputTargets = []
		for target in outputTargets:
			needNewTarget = False
			if target == reg:
				needNewTarget = True
			elif target.storage in (Storage.GLOBAL, Storage.IMM):
				needNewTarget = True
			
			if needNewTarget:
				target = state.findTarget(target.type, exclude=[reg, *inputTargets])
			
			inputTargets.append(target)
		
		state.blockInputs[ir.index] = inputTargets
		if isBrIf: state.blockInputs[ir.elseIndex] = inputTargets
	else:
		inputTargets = state.blockInputs[ir.index]
		for input, output, fType in zip(inputTargets, outputTargets, inputTypes):
			if input != output and input.active:
				target = state.findTarget(input.type, exclude=inputTargets)
				moveData(state, input, target, reg)
				state.moveOperand(input, target)
				assert input not in state.operandStack
			input.type = fType
		
		# output targets may have changed if "moveOperand" was called, so they must be reloaded
		outputTargets = list(reversed([state.getOperand(i + offset) for i in range(0, len(inputTypes))]))
	
	saveReg(state, reg, exclude=inputTargets)
	
	for src, dest in zip(outputTargets, inputTargets):
		assert src.type == dest.type
		if src == dest:
			continue
		moveData(state, src, dest, reg)
	
	if isBrIf:
		target = state.popOperand()
		if type(target) != RegTarget:
			reg.type = target.type
			state.appendInstr('mov', 
				Operand(reg, Usage.DEST), 
				Operand(target, Usage.SRC))
			target = reg
		
		state.appendInstr('test', 
			Operand(target, Usage.DEST), 
			Operand(target, Usage.SRC))
		
		state.appendInstr('jnz {}'.format(state.fnIR.blockDefs[ir.index].label))
		state.appendInstr('jmp {}'.format(state.fnIR.blockDefs[ir.elseIndex].label))
	else:
		state.appendInstr('jmp {}'.format(state.fnIR.blockDefs[ir.index].label))
	
	for _ in outputTargets:
		state.popOperand()

def br(state, ir):
	brOrBrIf(state, ir, False)

def brIf(state, ir):
	brOrBrIf(state, ir, True)
	
def ret(state, ir):
	if len(state.fnIR.retTypes) > 0:
		assert len(state.fnIR.retTypes) < 2
		src = state.getOperand(0)
		if src != state.rax:
			state.rax.setActive(False)
			state.rax.operandIndex = None
			state.rax.type = src.type
			state.appendInstr('mov', 
				Operand(state.rax, Usage.DEST), 
				Operand(src, Usage.SRC))
			state.popOperand()
			state.pushOperand(state.rax)
	
	state.appendInstr('jmp {}__ret'.format(state.fnIR.name))

def saveReg(state, src, exclude=[]):
	if not src.active:
		return
	
	dest = state.findTarget(src.type, True, exclude=exclude)
	state.moveOperand(src, dest)
	state.appendInstr('mov', 
		Operand(dest, Usage.DEST), 
		Operand(src, Usage.SRC))

def delcareExterns(mod, output):	
	for decl in mod.modDecls:
		delcareExterns(decl, output)
	
	for decl in mod.fnDecls:
		if decl.extern:
			output.write('extern {}\n'.format(decl.mangledName))

def declareFns(mod, output):
	for decl in mod.modDecls:
		declareFns(decl, output)
	
	for decl in mod.fnDecls:
		if not decl.extern:
			output.write('global {}\n'.format(decl.mangledName))

def defineStatics(mod, output):
	for decl in mod.modDecls:
		defineStatics(decl, output)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		
		for staticDef in decl.ir.staticDefs:
			bytes = ''.join([str(b) + ',' for b in staticDef.value])
			output.write('\t{}: db {}0\n'.format(staticDef.label, bytes))

def defineFns(mod, output):
	for decl in mod.modDecls:
		defineFns(decl, output)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		
		output.write('\t{}:\n'.format(decl.ir.name))
		
		state = GeneratorState(decl.ir)
		for instr in decl.ir.instr:
			irToAsm(state, instr)
		
		# stackOffset = 8 * len(stackArgs)
		# if stackOffset % 16 != 0:
		# 	stackOffset += 8
		# if stackOffset > 0:
		# 	state.appendInstr(Instr('\t\tadd rsp, {}\n'.format(stackOffset)))
		
		stackSize = state.sp * 8
		if stackSize % 16 == 0:
			stackSize += 8
		if stackSize > 0:
			output.write('\t\tsub rsp, {}\n'.format(stackSize))
		
		for instr in state.instr:
			output.write(instr.render(state.sp))
			output.write('\n')
		
		output.write('\t{}__ret:\n'.format(decl.ir.name))
		
		if stackSize > 0:
			output.write('\t\tadd rsp, {}\n'.format(stackSize))
		
		output.write('\t\tret\n')

import sys

class OutputWriter:
	def __init__(self):
		self.output = StringIO()
	
	def write(self, s):
		sys.stdout.write(s)
		self.output.write(s)
	
	def getvalue(self):
		return self.output.getvalue()

def generateAsm(mod):
	output = OutputWriter()
	
	delcareExterns(mod, output)
	
	if mod.mainFnDecl != None:
		output.write('\nglobal _start\n')
	
	declareFns(mod, output)
	
	output.write('\nsection .data\n')
	defineStatics(mod, output)
	
	output.write('\nsection .text\n')
	
	if mod.mainFnDecl != None:
		output.write('\t_start:\n')
		output.write('\t\tsub rsp, 8\n')
		output.write('\t\tcall {}\n'.format(mod.mainFnDecl.mangledName))
		output.write('\t\tmov rax, 0x02000001\n')
		output.write('\t\tmov rdi, 0\n')
		output.write('\t\tsyscall\n')
	
	defineFns(mod, output)
	
	return output.getvalue()

