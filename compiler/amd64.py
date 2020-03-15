import ctypes
from io                                import StringIO
from enum                              import Enum
from .ir                               import Dup, Global, Imm, Static, Deref, DerefW, Add, Sub, Mul, \
                                              Div, Eq, NEq, Less, LessEq, Greater, GreaterEq, Call, \
                                              Extend, IExtend, Truncate, FExtend, FTruncate, IToF, \
                                              UToF, FToI, FToU, Ret, BrIf, Br, Swap, Pop, Raise, \
                                              Struct, Field, FieldW, DerefField, DerefFieldW, \
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
		self.fixed = False
	
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
	
	def render(self, usage, fType, ind, sp):
		assert ind == None
		assert usage == Usage.SRC
		return '{} {}'.format(sizeInd(fType), self.value)

class GlobalTarget(Target):
	def __init__(self, fType, label):
		super().__init__(fType, Storage.GLOBAL)
		self.label = label
	
	def render(self, usage, fType, ind, sp):
		assert ind == None
		
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
	
	def render(self, usage, fType, ind, sp):
		addr = '[rsp + {}{}]'.format(self.offset, ind if ind else '')
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
	
	def render(self, usage, fType, ind, sp):
		if sp == None:
			addr = '[%% {}{} %%]'.format(self.offset, ind if ind else '')
		else:
			addr = '[rsp + {}{}]'.format(sp - self.offset, ind if ind else '')
		
		if usage == Usage.ADDR:
			return addr
		elif usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), addr)
		else:
			assert 0

class MultiStackTarget(Target):
	def __init__(self, fType, targets):
		super().__init__(fType, Storage.STACK)
		self.offset = targets[-1].offset
		self.targets = targets
	
	def setActive(self, set):
		self.active = set
		for t in self.targets:
			t.setActive(set)
	
	def render(self, usage, fType, ind, sp):
		if sp == None:
			addr = '[%% {}{} %%]'.format(self.offset, ind if ind else '')
		else:
			addr = '[rsp + {}{}]'.format(sp - self.offset, ind if ind else '')
		
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
	
	def render(self, usage, fType, ind, sp):
		assert ind == None
		
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
	def __init__(self, target, usage, fType=None, ind=None):
		self.target = target
		self.usage = usage
		self.type = fType if fType else target.type
		self.ind = ind
	
	def render(self, sp):
		ind = self.ind
		if ind:
			ind = ' + {}'.format(ind.value if ind.storage == Storage.IMM else ind.q)
		
		return self.target.render(self.usage, self.type, ind, sp)

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
		if type.byteSize % 8 != 0:
			count += 1
		
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
		assert not src.fixed
		
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
	elif type(ir) == Deref:
		deref(state, ir)
	elif type(ir) == DerefW:
		derefw(state, ir)
	elif type(ir) == Field:
		field(state, ir)
	elif type(ir) == DerefField:
		field(state, ir, deref=True)
	elif type(ir) == FieldW:
		fieldw(state, ir)
	elif type(ir) == DerefFieldW:
		fieldw(state, ir, deref=True)
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

class RestoreRegs:
	pass

def moveData(state, src, dest, 
	srcDeref=False, destDeref=False, srcOffset=None, destOffset=None, type=None):
	
	restore = RestoreRegs()
	restore.rax = None
	restore.rbp = None
	restore.r10 = None
	restore.r11 = None
	
	def moveToReg(target, t):
		reg = state.findReg(exclude=[src, dest, srcOffset, destOffset])
		if reg == None:
			if state.rax not in (src, dest, srcOffset, destOffset):
				restore.rax = saveReg(state, state.rax)
				reg = state.rax
			elif state.rbp not in (src, dest, srcOffset, destOffset):
				restore.rbp = saveReg(state, state.rbp)
				reg = state.rbp
			elif state.r10 not in (src, dest, srcOffset, destOffset):
				restore.r10 = saveReg(state, state.r10)
				reg = state.r10
			else:
				restore.r11 = saveReg(state, state.r11)
				reg = state.r11
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST, t), 
			Operand(target, Usage.SRC, t))
		return reg
	
	if not (srcDeref and destDeref) and type == None:
		type = dest.type if srcDeref else src.type
	
	assert type != None
	assert dest.storage in (Storage.REG, Storage.STACK) or destDeref
	
	if type.byteSize not in (1, 2, 4, 8):
		moveStruct(state, src, dest, srcDeref, destDeref, srcOffset, destOffset, type)
		return
	
	srcType = IPTR if srcDeref else type
	destType = IPTR if destDeref else type
	
	srcIsReg = src.storage == Storage.REG
	destIsReg = dest.storage == Storage.REG
	srcIsStk = src.storage == Storage.STACK
	destIsStk = dest.storage == Storage.STACK
	
	# setup source offset
	if srcOffset:
		if srcOffset.storage in (Storage.STACK, Storage.GLOBAL):
			srcOffset = moveToReg(srcOffset, IPTR)
		if srcDeref:
			if srcOffset.storage == Storage.IMM and src.storage == Storage.IMM:
				src = ImmTarget(IPTR, src.value + srcOffset.value)
				srcOffset = None
		else:
			assert src.storage == Storage.STACK
			if srcOffset.storage == Storage.IMM:
				src = StackTarget(src.type, src.offset + srcOffset.value)
				srcOffset = None
	
	# setup destination offset
	if destOffset:
		if destOffset.storage in (Storage.STACK, Storage.GLOBAL):
			destOffset = moveToReg(destOffset, IPTR)
		if destDeref:
			if destOffset.storage == Storage.IMM and dest.storage == Storage.IMM:
				dest = ImmTarget(IPTR, dest.value - destOffset.value)
				destOffset = None
		else:
			assert dest.storage == Storage.STACK
			if destOffset.storage == Storage.IMM:
				dest = StackTarget(dest.type, dest.offset - destOffset.value)
				destOffset = None
	
	# save source pointer
	# srcReg = None
	# restoreSrc = None
	# if src.active and srcIsReg and srcDeref:
	# 	srcReg = src
	# 	restoreSrc = saveReg(state, src)
	
	# move source to register if necessary
	if not srcIsReg and srcDeref or srcIsStk and (destIsStk or destDeref):
		src = moveToReg(src, srcType)
	
	# save destination pointer
	# destReg = None
	# restoreDest = None
	# if dest.active and destIsReg and destDeref:
	# 	destReg = dest
	# 	restoreDest = saveReg(state, dest)
	
	# move destination to register if necessary
	stkDest = None
	if not destIsReg and destDeref:
		dest = moveToReg(dest, destType)
	elif destIsStk and srcDeref:
		stkDest = dest
		dest = moveToReg(dest, destType, ind=destOffset)
	
	# move the data
	if srcDeref or destDeref:
		if srcDeref:
			if srcOffset:
				state.appendInstr('add', 
					Operand(src, Usage.DEST, IPTR), 
					Operand(srcOffset, Usage.SRC, IPTR))
				srcOffset = None
			if destDeref:
				state.appendInstr('mov', 
					Operand(src, Usage.DEST, type), 
					Operand(src, Usage.DEREF, type))
			else:
				state.appendInstr('mov', 
					Operand(dest, Usage.DEST, type, ind=destOffset), 
					Operand(src, Usage.DEREF, type))
		if destDeref:
			if destOffset:
				state.appendInstr('add', 
					Operand(dest, Usage.DEST, IPTR), 
					Operand(destOffset, Usage.SRC, IPTR))
			state.appendInstr('mov', 
				Operand(dest, Usage.DEREF, type), 
				Operand(src, Usage.SRC, type, ind=srcOffset))
	else:
		state.appendInstr('mov', 
			Operand(dest, Usage.DEST, type, ind=destOffset), 
			Operand(src, Usage.SRC, type, ind=srcOffset))
	
	# save destination to stack if necessary
	if stkDest:
		state.appendInstr('mov', 
			Operand(stkDest, Usage.DEST, type), 
			Operand(dest, Usage.SRC, type))
		dest = stkDest
	
	# restore registers
	if restore.rax:
		restoreReg(state, restore.rax, state.rax)
	if restore.rbp:
		restoreReg(state, restore.rbp, state.rbp)
	if restore.r10:
		restoreReg(state, restore.r10, state.r10)
	if restore.r11:
		restoreReg(state, restore.r11, state.r11)
	# if restoreSrc:
	# 	restoreReg(state, restoreSrc, srcReg)
	# if restoreDest:
	# 	restoreReg(state, restoreDest, destReg)
	
	dest.type = destType

def moveStruct(state, src, dest, srcDeref, destDeref, srcOffset, destOffset, type):
	assert srcDeref or src.storage == Storage.STACK
	assert destDeref or dest.storage == Storage.STACK
	
	partialDest = dest
	partialSrc = src
	sizeRemaining = type.byteSize
	offset = 0
	while sizeRemaining > 0:
		if sizeRemaining >= 8:
			byteSize = 8
		elif sizeRemaining >= 4:
			byteSize = 4
		elif sizeRemaining >= 2:
			byteSize = 2
		else:
			byteSize = 1
		
		partialType = FundamentalType(byteSize)
		
		if not srcDeref:
			partialSrc = StackTarget(partialType, src.offset - offset)
		if not destDeref:
			partialDest = StackTarget(partialType, dest.offset - offset)
		
		moveData(state, partialSrc, partialDest, srcDeref, destDeref, srcOffset, destOffset, partialType)
		offset += byteSize
		sizeRemaining -= byteSize
		
		if sizeRemaining > 0:
			if srcDeref:
				state.appendInstr('add', 
					Operand(partialSrc, Usage.DEST, IPTR), 
					Operand(ImmTarget(IPTR, byteSize), Usage.SRC, IPTR))
			if destDeref:
				state.appendInstr('add', 
					Operand(partialDest, Usage.DEST, IPTR), 
					Operand(ImmTarget(IPTR, byteSize), Usage.SRC, IPTR))
	
	dest.type = IPTR if destDeref else type

def dup(state, ir):
	src = state.getOperand(ir.offset)
	
	if src.storage == Storage.GLOBAL:
		dest = GlobalTarget(src.type, src.label)
	elif type(ir) == Imm:
		dest = ImmTarget(src.type, src.value)
	else:
		dest = state.findTarget(src.type)
		moveData(state, src, dest)
	
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
		moveData(state, src, newSrc)
		src = newSrc
	
	src.fixed = True
	
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
	src = state.getOperand(0)
	dest = state.findTarget(ir.type)
	moveData(state, src, dest, srcDeref=True)
	
	state.popOperand()
	state.pushOperand(dest)

def derefw(state, ir):
	src = state.getOperand(0)
	dest = state.getOperand(1)
	moveData(state, src, dest, destDeref=True)
	
	state.popOperand()
	state.popOperand()

def field(state, ir, deref=False):
	offsetTarget = state.getOperand(0)
	structTarget = state.getOperand(ir.offset)
	readTarget = state.findTarget(ir.type)
	
	moveData(state, structTarget, readTarget, srcOffset=offsetTarget, srcDeref=deref)
	
	state.popOperand()
	state.pushOperand(readTarget)
	
	# fieldTarget = None
	# if offsetTarget.storage == Storage.IMM:
	# 	structTarget = state.getOperand(ir.offset)
		
	# 	if deref:
	# 		if offsetTarget.storage == Storage.IMM:
	# 			fieldTarget = StackTarget(ir.type, structTarget.value - offsetTarget.value)
	# 		else:
	# 			if structTarget.storage != Storage.REG:
	# 				moveData(state, structTarget, reg)
	# 				structTarget = reg
			
	# 			state.appendInstr('add', 
	# 				Operand(structTarget, Usage.DEST, IPTR), 
	# 				Operand(ImmTarget(IPTR, offsetTarget.value), Usage.SRC, IPTR))
	# 			fieldTarget = structTarget
	# 	else:
	# 		fieldTarget = StackTarget(ir.type, structTarget.offset - offsetTarget.value)
		
	# 	readTarget = state.findTarget(ir.type)
	# 	moveData(state, fieldTarget, readTarget, srcDeref=deref)
	# else:
	# 	if offsetTarget.storage != Storage.REG:
	# 		moveData(state, offsetTarget, reg)
	# 		offsetTarget = reg
		
	# 	structTarget = state.getOperand(ir.offset)
	# 	assert structTarget.storage == Storage.STACK
		
	# 	readTarget = state.findTarget(ir.type)
	# 	state.appendInstr('mov', 
	# 		Operand(readTarget, Usage.DEST, IPTR), 
	# 		Operand(structTarget, Usage.ADDR, IPTR, ind=offsetTarget))

def fieldw(state, ir, deref=False):
	offsetTarget = state.getOperand(0)
	valueTarget = state.getOperand(1)
	structTarget = state.getOperand(ir.offset)
	
	if structTarget.storage == Storage.NONE:
		newStructTarget = state.findTarget(structTarget.type, True)
		state.moveOperand(structTarget, newStructTarget)
		structTarget = newStructTarget
	
	moveData(state, valueTarget, structTarget, destOffset=offsetTarget, srcDeref=deref)
	
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
			stack = saveReg(state, state.rax)
			state.moveOperand(state.rax, stack)
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
	restoreRAX = None
	restoreRBP = None
	
	src = state.getOperand(0)
	dest = state.getOperand(1)
	
	if src.storage == Storage.IMM and dest.storage == Storage.IMM:
		state.popOperand()
		dest.value = (dest.value + src.value) if isAdd else (dest.value - src.value)
		return
	elif dest.storage == Storage.IMM and isAdd:
		dest, src = src, dest
	
	if src.storage == Storage.GLOBAL or Storage.REG not in (dest.storage, src.storage):
		reg = state.findReg()
		if reg == None:
			if dest == state.rax:
				restoreRBP = saveReg(state, state.rbp)
				reg = state.rbp
			else:
				restoreRAX = saveReg(state, state.rax)
				reg = state.rax
		reg.type = src.type
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST), 
			Operand(src, Usage.SRC))
		src = reg
	
	assert dest.storage in (Storage.REG, Storage.STACK)
	assert Storage.REG in (dest.storage, src.storage)
	
	state.appendInstr('add' if isAdd else 'sub',
			Operand(dest, Usage.DEST), 
			Operand(src, Usage.SRC))
	
	if restoreRAX:
		restoreReg(state, restoreRAX, state.rax)
	
	if restoreRBP:
		restoreReg(state, restoreRBP, state.rbp)
	
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
		dest.value = (dest.value * src.value) if isMul else (dest.value // src.value)
		return
	
	if isMul and src == state.rax:
		dest, src = src, dest
	
	if src != state.rdx:
		stack = saveReg(state, state.rdx)
		state.moveOperand(state.rdx, stack)
	
	if dest != state.rax:
		stack = saveReg(state, state.rax)
		state.moveOperand(state.rax, stack)
		state.rax.type = dest.type
		state.appendInstr('mov', 
			Operand(state.rax, Usage.DEST), 
			Operand(dest, Usage.SRC))
		dest = state.rax
		src = state.getOperand(0) # in case src was rax
	
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
	if state.rcx.active:
		stack = saveReg(state, state.rcx)
		state.moveOperand(state.rcx, stack)
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
		stack = saveReg(state, state.rax)
		state.moveOperand(state.rax, stack)
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
	for reg in state.intRegs:
		if reg.calleeSaved:
			continue
		if reg.active:
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
	
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
			if reg.active:
				stack = saveReg(state, reg)
				state.moveOperand(reg, stack)
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
			moveData(state, src, TopStackTarget(src.type, i * 8))
	
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
	testReg = None
	blockDef = state.fnIR.blockDefs[ir.index]
	inputTypes = blockDef.inputs
	outputTargets = list(reversed([state.getOperand(i + offset) for i in range(0, len(inputTypes))]))
	
	if isBrIf:
		testReg = state.getOperand(0)
		if testReg.storage != Storage.REG:
			testReg = state.r11
	
	if state.blockInputs[ir.index] == None:
		inputTargets = []
		for target in outputTargets:
			needNewTarget = False
			if target == testReg:
				needNewTarget = True
			elif target.storage in (Storage.GLOBAL, Storage.IMM):
				needNewTarget = True
			
			if needNewTarget:
				target = state.findTarget(target.type, exclude=[testReg, *inputTargets])
			
			inputTargets.append(target)
		
		state.blockInputs[ir.index] = inputTargets
		if isBrIf: state.blockInputs[ir.elseIndex] = inputTargets
	else:
		inputTargets = state.blockInputs[ir.index]
		for input, output, fType in zip(inputTargets, outputTargets, inputTypes):
			if input != output and input.active:
				target = state.findTarget(input.type, exclude=inputTargets)
				moveData(state, input, target)
				state.moveOperand(input, target)
				assert input not in state.operandStack
			input.type = fType
		
		# output targets may have changed if "moveOperand" was called, so they must be reloaded
		outputTargets = list(reversed([state.getOperand(i + offset) for i in range(0, len(inputTypes))]))
	
	for src, dest in zip(outputTargets, inputTargets):
		assert src.type == dest.type
		if src == dest:
			continue
		
		moveData(state, src, dest)
		dest.setActive(True) # prevents later calls to moveData from clobbering this dest
	
	for dest in inputTargets:
		dest.setActive(False)
	
	if isBrIf:
		target = state.popOperand()
		if target.storage != Storage.REG:
			if testReg.active:
				stack = saveReg(state, testReg)
				state.moveOperand(testReg, stack)
			testReg.type = target.type
			state.appendInstr('mov', 
				Operand(testReg, Usage.DEST), 
				Operand(target, Usage.SRC))
			target = testReg
		
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

def saveReg(state, reg, exclude=[]):
	assert reg.active
	
	stack = state.findTarget(reg.type, True, exclude=exclude)
	state.appendInstr('mov', 
		Operand(stack, Usage.DEST), 
		Operand(reg, Usage.SRC))
	
	return stack

def restoreReg(state, stack, reg):
	reg.type = stack.type
	state.appendInstr('mov', 
		Operand(reg, Usage.DEST), 
		Operand(stack, Usage.SRC))

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
		
		stackSize = state.sp
		if stackSize % 16 == 0:
			stackSize += 8
		if stackSize > 0:
			output.write('\t\tsub rsp, {}\n'.format(stackSize))
		
		for instr in state.instr:
			output.write(instr.render(stackSize))
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
		# sys.stdout.write(s)
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

