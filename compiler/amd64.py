import ctypes
from io     import StringIO
from enum   import Enum
from .ast   import StaticData, StaticDataType
from .ir    import Dup, Global, Imm, Static, Deref, DerefW, Add, Sub, Mul, Div, Mod, Eq, NEq, Less, LessEq, \
                   Greater, GreaterEq, FAdd, FSub, FMul, FDiv, FEq, FNEq, FLess, FLessEq, FGreater, FGreaterEq, \
                   Call, Extend, IExtend, Truncate, FExtend, FTruncate, IToF, UToF, FToI, FToU, Ret, BrIf, \
                   Br, Swap, Write, Pop, Raise, Neg, Res, Field, FieldW, DerefField, DerefFieldW, Fix, Addr, \
                   I8, I16, I32, I64, IPTR, F32, F64, FundamentalType, BlockMarker
from .types import U32_RNG, I32_RNG
from .      import platform

class Storage(Enum):
	IMM = 'IMM'
	GLOBAL = 'GLOBAL'
	REG = 'REG'
	XMM = 'XMM'
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
	
	def refreshOffset(self):
		pass
		
	def setActive(self, set):
		self.active = set
	
	def render(self, usage, fType, sp):
		assert 0
	
	@staticmethod
	def fromIR(ir):
		if type(ir) == Global or type(ir) == Static:
			return GlobalTarget(ir.type, ir.label)
		elif type(ir) == Res:
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
		assert self.label
		assert ind == None
		
		if usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), self.label)
		elif usage == Usage.ADDR:
			return '[rel {}]'.format(self.label)
		else:
			assert 0

class StackTarget(Target):
	def __init__(self, fType, offset, fromTop=False):
		super().__init__(fType, Storage.STACK)
		self.offset = offset
		self.fromTop = fromTop
	
	def render(self, usage, fType, ind, sp):
		offset = self.offset
		if ind and ind.storage == Storage.IMM:
			offset += ind.value
			ind = ''
		else:
			ind = ' + {}'.format(ind.q) if ind else ''
		
		if self.fromTop:
			offset = ' + {}'.format(offset) if offset > 0 else ''
			addr = '[rsp{}{}]'.format(offset, ind)
		elif sp == None:
			addr = '[%% {}{} %%]'.format(offset, ind)
		else:
			offset = ' + {}'.format(sp - offset) if sp - offset > 0 else ''
			addr = '[rsp{}{}]'.format(offset, ind)
		
		if usage == Usage.ADDR:
			return addr
		elif usage in (Usage.SRC, Usage.DEST):
			return '{} {}'.format(sizeInd(fType), addr)
		else:
			assert 0

class MultiStackTarget(StackTarget):
	def __init__(self, fType, targets):
		super().__init__(fType, targets[-1].offset)
		self.targets = targets
	
	def refreshOffset(self):
		self.offset = self.targets[-1].offset
	
	def setActive(self, set):
		self.active = set
		for t in self.targets:
			t.setActive(set)

class RegTarget(Target):
	def __init__(self, calleeSaved, q, d, w, l, h=None):
		super().__init__(None, Storage.REG)
		self.calleeSaved = calleeSaved
		self.l = l; self.h = h
		self.w = w; self.d = d
		self.q = q
	
	def render(self, usage, fType, ind, sp):
		assert usage == Usage.DEREF or ind == None
		
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
			ind = '' if ind == None else ' + {}'.format(ind.value if ind.storage == Storage.IMM else ind.q)
			return '{} [{}{}]'.format(sizeInd(fType), s, ind)
		elif usage in (Usage.SRC, Usage.DEST):
			return s
		else:
			assert 0

class XmmTarget(Target):
	def __init__(self, index):
		super().__init__(None, Storage.XMM)
		self.index = index
		self.name = 'xmm{}'.format(index)
	
	def render(self, usage, fType, ind, sp):
		assert usage in (Usage.SRC, Usage.DEST) and ind == None
		return self.name

class Operand:
	def __init__(self, target, usage, fType=None, ind=None):
		self.target = target
		self.usage = usage
		self.type = fType if fType else target.type
		self.ind = ind
	
	def render(self, sp):
		return self.target.render(self.usage, self.type, self.ind, sp)

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

class GeneratorSettings:
	def __init__(self):
		self.omitFramePointer = True
		self.stream = False

class GeneratorState:
	def __init__(self, fnIR):
		self.settings = GeneratorSettings()
		
		self.fnIR = fnIR
		self.blockDef = None
		self.blockInputs = [None for _ in fnIR.blockDefs]
		self.instr = []
		self.operandStack = []
		self.calleeSave = set()
		
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
			self.r11, self.r10, self.rdi, self.rsi, self.rdx, 
			self.rcx,  self.r8, self.rbx, self.rbp, self.r12, 
			self.r13, self.r14, self.r15,  self.r9, self.rax,
			self.rbx, self.r12, self.r13, self.r14, self.r15
		]
		self.intArgRegs = [
			self.rdi, self.rsi, self.rdx, self.rcx, self.r8, 
			self.r9
		]
		self.xmm0  = XmmTarget(0);  self.xmm1  = XmmTarget(1)
		self.xmm2  = XmmTarget(2);  self.xmm3  = XmmTarget(3)
		self.xmm4  = XmmTarget(4);  self.xmm5  = XmmTarget(5)
		self.xmm6  = XmmTarget(6);  self.xmm7  = XmmTarget(7)
		self.xmm8  = XmmTarget(8);  self.xmm9  = XmmTarget(9)
		self.xmm10 = XmmTarget(10); self.xmm11 = XmmTarget(11)
		self.xmm12 = XmmTarget(12); self.xmm13 = XmmTarget(13)
		self.xmm14 = XmmTarget(14); self.xmm15 = XmmTarget(15)
		self.xmmRegs = [
			self.xmm0,   self.xmm1,  self.xmm2,  self.xmm3,
			self.xmm4,   self.xmm5,  self.xmm6,  self.xmm7,
			self.xmm8,   self.xmm9, self.xmm10, self.xmm11,
			self.xmm12, self.xmm13, self.xmm14, self.xmm15,
		]
		self.xmmArgRegs = [
			self.xmm0,   self.xmm1,  self.xmm2,  self.xmm3,
			self.xmm4,   self.xmm5,  self.xmm6,  self.xmm7,
		]
		self.sp = 0
		self.callStack = []
	
	def generateAsm(self, output):
		assert not (self.settings.stream and self.settings.omitFramePointer)
		
		output.write('\t{}:\n'.format(self.fnIR.name))
		
		# if not self.settings.omitFramePointer:
		# 	output.write('\t\tpush rbp\n')
		# 	output.write('\t\tmov rbp, rsp\n')
		
		self.setupStackFrame()
		
		irInstrs = self.fnIR.instr
		if len(irInstrs) > 0:
			ir = irInstrs[0]
			for i in range(1, len(irInstrs)):
				nextIR = irInstrs[i]
				irToAsm(self, ir, nextIR)
				ir = nextIR
			irToAsm(self, ir, None)
		
		calleeSaveList = list(self.calleeSave)
		
		if True:#self.settings.omitFramePointer:
			self.sp += len(self.calleeSave) * 8
			if self.sp % 16 == 0:
				self.sp += 8
			if self.sp > 0:
				output.write('\t\tsub rsp, {}\n'.format(self.sp))
			
			for (i, reg) in enumerate(calleeSaveList):
				output.write('\t\tmov [rsp + {}], {}\n'.format(self.sp - i*8, reg.q))
		
		for instr in self.instr:
			output.write(instr.render(self.sp))
			output.write('\n')
		
		output.write('\t{}__ret:\n'.format(self.fnIR.name))
		
		if self.sp > 0:
			for (i, reg) in reversed(list(enumerate(calleeSaveList))):
				output.write('\t\tmov {}, [rsp + {}]\n'.format(reg.q, self.sp - i*8))
			
			output.write('\t\tadd rsp, {}\n'.format(self.sp))
		
		output.write('\t\tret\n')
	
	def allocateStack(self, count=1):
		targets = []
		for _ in range(0, count):
			self.sp += 8
			target = StackTarget(None, self.sp)
			self.callStack.append(target)
			targets.append(target)
		
		return targets
	
	def setupStackFrame(self):
		assert not self.fnIR.cVarArgs
		
		memStructReturn = False
		if self.fnIR.retType and assignTargets(self, [self.fnIR.retType], ret=True)[0] == None:
			memStructReturn = True
			self.rdi.type = I64
			self.pushOperand(self.rdi)
		
		targets = [None for _ in range(0, len(self.fnIR.paramTypes))]
		assignments = assignTargets(self, self.fnIR.paramTypes, memStructReturn=memStructReturn)
		stackTargets = []
		structParts = {}
		for (i, (t, regs)) in enumerate(zip(self.fnIR.paramTypes, assignments)):
			if regs == None:
				multiTargets = []
				offset = 0
				while offset < t.byteSize:
					multiTargets.append(StackTarget(None, 0))
					offset += 8
				
				# multiTargets.reverse()
				target = multiTargets[0] if len(multiTargets) == 1 else MultiStackTarget(None, multiTargets)
				target.type = t
				targets[i] = target
				stackTargets.extend(reversed(multiTargets))
			elif len(regs) == 1:
				r = regs[0]
				r.type = t
				targets[i] = r
			else:
				structParts[i] = (t, regs)
		
		offset = -8 * len(stackTargets)
		for target in reversed(stackTargets):
			target.offset = offset
			offset += 8
			self.callStack.append(target)
		
		retAddr = StackTarget(I64, 0)
		retAddr.setActive(True)
		self.callStack.append(retAddr)
		
		self.sp = 0
		
		for i in structParts:
			(t, regs) = structParts[i]
			target = self.findTarget(t, True)
			target.setActive(True)
			targets[i] = target
			for (j, reg) in enumerate(regs):
				offset = ImmTarget(I64, j*8)
				moveData(self, reg, target, destOffset=offset, type=I64)
		
		for target in targets:
			target.setActive(False)
			target.refreshOffset()
			self.pushOperand(target)
	
	def appendInstr(self, opcode, *operands, isLabel=False, isComment=False):
		instr = Instr(opcode, operands, isLabel, isComment)
		self.instr.append(instr)
		# print(instr)
	
	def findReg(self, type=None, exclude=[]):
		for reg in self.intRegs:
			if reg.active or reg in exclude:
				continue
			if reg.calleeSaved:
				self.calleeSave.add(reg)
			if type:
				reg.type = type
			return reg
	
	def findXmmReg(self, type=None, exclude=[]):
		for reg in self.xmmRegs:
			if reg.active or reg in exclude:
				continue
			if type:
				reg.type = type
			return reg
	
	def findTarget(self, type, alwaysUseStack=False, exclude=[]):
		if type.byteSize not in (1, 2, 4, 8):
			alwaysUseStack = True
		
		if not alwaysUseStack:
			target = self.findReg(type, exclude=exclude)
			if target:
				return target
		
		offset = 0
		count = type.byteSize // 8
		if type.byteSize % 8 != 0:
			count += 1
		
		targets = None
		while offset < len(self.callStack):
			targets = []
			for i in range(0, count):
				t = self.callStack[offset + i] if offset + i < len(self.callStack) else None
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
		
		if count == 1:
			target = targets[0]
			target.type = type
			return target
		
		for t in targets:
			t.type = None
		
		return MultiStackTarget(type, targets)
	
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
		
		other = self.operandStack[index]
		other.setActive(False)
		other.operandIndex = None
		target = self.operandStack.pop()
		target.operandIndex = index
		self.operandStack[index] = target
		self.pushOperand(other)
	
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

def irToAsm(state, ir, nextIR):
	# state.appendInstr(ir.pretty(state.fnIR), isComment=True, isLabel=(type(ir) == BlockMarker))
	
	if type(ir) == Imm:
		imm(state, ir)
	elif type(ir) in (Global, Static, Res):
		state.pushIROperand(ir)
	elif type(ir) == Pop:
		state.popOperand()
	elif type(ir) == Dup:
		dup(state, ir)
	elif type(ir) == Swap:
		swap(state, ir)
	elif type(ir) == Write:
		write(state, ir)
	elif type(ir) == Raise:
		raise_(state, ir)
	elif type(ir) == Fix:
		fix(state, ir)
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
	elif type(ir) == FExtend:
		fextend(state, ir)
	elif type(ir) == Truncate:
		truncate(state, ir)
	elif type(ir) == IToF:
		itof(state, ir)
	elif type(ir) == FToI:
		ftoi(state, ir)
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
	elif type(ir) == Mod:
		mod(state, ir)
	elif type(ir) in (Eq, NEq, Less, LessEq, Greater, GreaterEq):
		cmp(state, ir)
	elif type(ir) == FAdd:
		fadd(state, ir)
	elif type(ir) == FSub:
		fsub(state, ir)
	elif type(ir) == FMul:
		fmul(state, ir)
	elif type(ir) == FDiv:
		fdiv(state, ir)
	elif type(ir) in (FEq, FNEq, FLess, FLessEq, FGreater, FGreaterEq):
		fcmp(state, ir)
	elif type(ir) == Call:
		call(state, ir)
	elif type(ir) == BlockMarker:
		blockMarker(state, ir)
	elif type(ir) == BrIf:
		brIf(state, ir, nextIR)
	elif type(ir) == Br:
		br(state, ir, nextIR)
	elif type(ir) == Ret:
		ret(state, ir)
	else:
		assert 0
	
	# state.appendInstr('[{}]'.format(', '.join(str(t.type) for t in state.operandStack)), 
		# isComment=True, isLabel=(type(ir) == BlockMarker))

class RestoreRegs:
	pass

def moveData(state, src, dest, 
	srcDeref=False, destDeref=False, srcOffset=None, destOffset=None, type=None, preserveDestType=False):
	
	restore = RestoreRegs()
	restore.r8 = None
	restore.r9 = None
	restore.r10 = None
	restore.r11 = None
	
	def getReg():
		reg = state.findReg(exclude=[src, dest, srcOffset, destOffset])
		if reg == None:
			if state.r8 not in (src, dest, srcOffset, destOffset):
				restore.r8 = saveReg(state, state.r8)
				reg = state.r8
			elif state.r9 not in (src, dest, srcOffset, destOffset):
				restore.r9 = saveReg(state, state.r9)
				reg = state.r9
			elif state.r10 not in (src, dest, srcOffset, destOffset):
				restore.r10 = saveReg(state, state.r10)
				reg = state.r10
			else:
				restore.r11 = saveReg(state, state.r11)
				reg = state.r11
		return reg
	
	def moveToReg(target, t):
		reg = getReg()
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST, t), 
			Operand(target, Usage.SRC, t))
		return reg
	
	if type == None and not (srcDeref and destDeref):
		if srcDeref:
			type = dest.type
		elif destDeref:
			type = src.type
		else:
			type = src.type if not dest.type or src.type and src.type.byteSize < dest.type.byteSize else dest.type
	
	assert type != None
	assert dest.storage in (Storage.REG, Storage.XMM, Storage.STACK) or destDeref
	
	if src.storage == Storage.NONE:
		newSrc = state.findTarget(src.type)
		state.moveOperand(src, newSrc)
		src = newSrc
	
	if type.byteSize not in (1, 2, 4, 8):
		if src.storage == Storage.REG:
			src.type = I64
			stack = saveReg(state, src)
			state.moveOperand(src, stack)
			src = stack
		moveStruct(state, src, dest, srcDeref, destDeref, srcOffset, destOffset, type, preserveDestType)
		return
	
	srcType = IPTR if srcDeref else type
	destType = IPTR if destDeref else type
	
	srcIsReg = src.storage == Storage.REG
	destIsReg = dest.storage == Storage.REG
	srcIsStk = src.storage == Storage.STACK
	destIsStk = dest.storage == Storage.STACK
	
	opcode = 'mov'
	if Storage.XMM in (src.storage, dest.storage):
		opcode += 'q' if type.byteSize == 8 else 'd'
	
	# setup source offset
	if srcOffset:
		if srcOffset.storage in (Storage.STACK, Storage.GLOBAL):
			srcOffset = moveToReg(srcOffset, IPTR)
		if srcDeref:
			if srcOffset.storage == Storage.IMM and src.storage == Storage.IMM:
				src = ImmTarget(IPTR, src.value + srcOffset.value)
				srcOffset = None
		else:
			if src.storage != Storage.STACK:
				stack = state.findTarget(src.type, True)
				moveData(state, src, stack)
				src = stack
			if srcOffset.storage == Storage.IMM:
				assert not src.fromTop
				src = StackTarget(src.type, src.offset - srcOffset.value)
				srcOffset = None
	
	# setup destination offset
	if destOffset:
		if destOffset.storage in (Storage.STACK, Storage.GLOBAL):
			destOffset = moveToReg(destOffset, IPTR)
		if destDeref:
			if destOffset.storage == Storage.IMM and dest.storage == Storage.IMM:
				dest = ImmTarget(IPTR, dest.value + destOffset.value)
				destOffset = None
		else:
			assert dest.storage == Storage.STACK
			if destOffset.storage == Storage.IMM:
				if dest.fromTop:
					dest = StackTarget(dest.type, dest.offset + destOffset.value, fromTop=True)
				else:
					dest = StackTarget(dest.type, dest.offset - destOffset.value)
				destOffset = None
	
	# save source pointer
	srcReg = None
	restoreSrc = None
	if src.active and srcIsReg and srcDeref and destDeref:
		srcReg = src
		restoreSrc = saveReg(state, src)
	
	# move source to register if necessary
	if (not srcIsReg and srcDeref) or (srcIsStk and (destIsStk or destDeref)) or \
		(src.storage == Storage.IMM and dest.storage == Storage.XMM) or \
		(src.storage == Storage.GLOBAL and not destIsReg):
		if srcIsStk and srcOffset and not srcDeref:
			reg = getReg()
			state.appendInstr('mov', 
				Operand(reg, Usage.DEST, type), 
				Operand(src, Usage.SRC, type, ind=srcOffset))
			srcOffset = None
			src = reg
		else:
			src = moveToReg(src, srcType)
	
	# move destination to register if necessary
	stkDest = None
	if not destIsReg and destDeref:
		dest = moveToReg(dest, destType)
	elif destIsStk and srcDeref:
		stkDest = dest
		dest = getReg()
	
	# move the data
	if srcDeref or destDeref:
		if srcDeref:
			if destDeref:
				state.appendInstr('mov', 
					Operand(src, Usage.DEST, type), 
					Operand(src, Usage.DEREF, type, ind=srcOffset))
			else:
				state.appendInstr(opcode, 
					Operand(dest, Usage.DEST, type), 
					Operand(src, Usage.DEREF, type, ind=srcOffset))
		if destDeref:
			state.appendInstr(opcode, 
				Operand(dest, Usage.DEREF, type, ind=destOffset), 
				Operand(src, Usage.SRC, type))
	else:
		state.appendInstr(opcode, 
			Operand(dest, Usage.DEST, type, ind=destOffset), 
			Operand(src, Usage.SRC, type, ind=srcOffset))
	
	# save destination to stack if necessary
	if stkDest:
		state.appendInstr('mov', 
			Operand(stkDest, Usage.DEST, type, ind=destOffset), 
			Operand(dest, Usage.SRC, type))
		dest = stkDest
	
	# restore registers
	if restore.r8:
		restoreReg(state, restore.r8, state.r8)
	if restore.r9:
		restoreReg(state, restore.r9, state.r9)
	if restore.r10:
		restoreReg(state, restore.r10, state.r10)
	if restore.r11:
		restoreReg(state, restore.r11, state.r11)
	if restoreSrc:
		restoreReg(state, restoreSrc, srcReg)
	
	if not preserveDestType:
		dest.type = destType

def moveStruct(state, src, dest, srcDeref, destDeref, srcOffset, destOffset, type, preserveDestType):
	assert srcDeref or src.storage == Storage.STACK
	assert destDeref or dest.storage == Storage.STACK
	
	if srcOffset == None:
		srcOffset = ImmTarget(IPTR, 0)
	if destOffset == None:
		destOffset = ImmTarget(IPTR, 0)
	
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
			partialSrcOffset = src.offset + offset if src.fromTop else src.offset - offset
			partialSrc = StackTarget(partialType, partialSrcOffset, fromTop=src.fromTop)
		if not destDeref:
			partialDestOffset = dest.offset + offset if dest.fromTop else dest.offset - offset
			partialDest = StackTarget(partialType, partialDestOffset, fromTop=dest.fromTop)
		
		moveData(state, partialSrc, partialDest, srcDeref, destDeref, srcOffset, destOffset, partialType)
		offset += byteSize
		sizeRemaining -= byteSize
		
		if sizeRemaining > 0:
			if srcDeref:
				srcOffset = ImmTarget(IPTR, srcOffset.value + byteSize)
			if destDeref:
				destOffset = ImmTarget(IPTR, destOffset.value + byteSize)
	
	if not preserveDestType:
		dest.type = IPTR if destDeref else type

FLOAT_COUNTER = 0

def imm(state, ir):
	if ir.type.isFloatType:
		global FLOAT_COUNTER
		label = '{}_float{}'.format(state.fnIR.name, FLOAT_COUNTER)
		FLOAT_COUNTER += 1
		
		staticValue = StaticData(ir.value, StaticDataType.FLOAT, ir.type, label)
		state.fnIR.staticDefs.append(staticValue)
		
		reg = state.findXmmReg(ir.type)
		if reg == None:
			stack = saveReg(state.xmm8)
			state.moveOperand(state.xmm8, stack)
			reg = state.xmm8
		
		state.appendInstr('movs{}'.format('d' if ir.type.byteSize == 8 else 's'), 
			Operand(reg, Usage.DEST),
			Operand(GlobalTarget(ir.type, label), Usage.ADDR))
		
		state.pushOperand(reg)
	elif ir.value not in U32_RNG and ir.value not in I32_RNG:
		# 64-bit immediates must be moved through a register
		reg = state.findReg()
		if reg == None:
			stack = saveReg(state.rax)
			state.moveOperand(state.rax, stack)
			reg = state.rax
		reg.type = I64
		state.appendInstr('mov', 
			Operand(reg, Usage.DEST, I64),
			Operand(Target.fromIR(ir), Usage.SRC))
		state.pushOperand(reg)
	else:
		state.pushIROperand(ir)

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

def fix(state, ir):
	src = state.getOperand(ir.offset)
	if src.storage != Storage.STACK:
		newSrc = state.findTarget(src.type, True)
		state.moveOperand(src, newSrc)
		if src.storage != Storage.NONE:
			moveData(state, src, newSrc)
		src = newSrc
	
	src.fixed = True

def write(state, ir):
	src = state.getOperand(0)
	dest = state.getOperand(ir.offset)
	moveData(state, src, dest)
	state.popOperand()

def addr(state, ir):
	src = state.getOperand(ir.offset)
	assert src.storage == Storage.STACK and src.fixed
	
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

def fieldw(state, ir, deref=False):
	offsetTarget = state.getOperand(0)
	valueTarget = state.getOperand(1)
	structTarget = state.getOperand(ir.offset)
	
	if structTarget.storage == Storage.NONE:
		newStructTarget = state.findTarget(structTarget.type, True)
		state.moveOperand(structTarget, newStructTarget)
		structTarget = newStructTarget
	
	moveData(state, valueTarget, structTarget, destOffset=offsetTarget, destDeref=deref, preserveDestType=True)
	
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
	
	if signed:
		state.appendInstr('movsxd' if src.type.byteSize == 4 else 'movsx', 
			Operand(dest, Usage.DEST, ir.type), 
			Operand(src, Usage.SRC))
	elif src.type.byteSize < 4:
		state.appendInstr('movzx', 
			Operand(dest, Usage.DEST, ir.type), 
			Operand(src, Usage.SRC))
	
	dest.type = ir.type
	
	state.popOperand()
	state.pushOperand(dest)

def iextend(state, ir):
	extend(state, ir, True)

def fextend(state, ir):
	src = state.getOperand(0)
	dest = src
	
	if dest.storage != Storage.XMM:
		reg = state.findXmmReg()
		if reg == None:
			reg = state.xmm8
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
		dest = reg
	
	if src.storage == Storage.REG:
		moveData(state, src, dest)
		dest.type = src.type
		src = dest
	
	assert ir.type == F64 and src.type == F32
	
	state.appendInstr('cvtss2sd', 
		Operand(dest, Usage.DEST), 
		Operand(src, Usage.SRC))
	
	dest.type = ir.type
	
	state.popOperand()
	state.pushOperand(dest)

def truncate(state, ir):
	state.getOperand(0).type = ir.type

def itof(state, ir):
	src = state.getOperand(0)
	
	if src.type.byteSize < 4:
		iextend(state, IExtend(ir.ast, I32))
		src = state.getOperand(0)
	
	dest = src
	if dest.storage != Storage.XMM:
		dest = state.findXmmReg(dest.type)
		if dest == None:
			reg = state.xmm8
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
			reg.type = dest.type
			dest = reg
	
	if src.storage == Storage.IMM:
		reg = state.findReg(src.type)
		if src == None:
			reg = state.rax
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
			reg.type = src.type
		moveData(state, src, reg)
		src = reg
	
	state.appendInstr('cvtsi2sd' if ir.type == F64 else 'cvtsi2ss',
			Operand(dest, Usage.DEST, ir.type), 
			Operand(src, Usage.SRC))
	
	dest.type = ir.type
	state.popOperand()
	state.pushOperand(dest)

def ftoi(state, ir):
	assert 0

def neg(state, ir):
	target = state.getOperand(0)
	if target.storage == Storage.IMM:
		target.value = -src.value
	else:
		state.appendInstr('neg', Operand(target, Usage.SRC))

def addOrSub(state, ir, isAdd):
	restoreRAX = None
	restoreR9 = None
	
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
				restoreR9 = saveReg(state, state.r9)
				reg = state.r9
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
	
	if restoreR9:
		restoreReg(state, restoreRDI, state.r9)
	
	state.popOperand()
	state.popOperand()
	state.pushOperand(dest)

def add(state, ir):
	addOrSub(state, ir, True)

def sub(state, ir):
	addOrSub(state, ir, False)

def mulDivMod(state, ir, isMul, isMod):
	src = state.getOperand(0)
	dest = state.getOperand(1)
	
	if src.storage == Storage.IMM and dest.storage == Storage.IMM:
		state.popOperand()
		if isMul:
			dest.value = dest.value * src.value
		elif isMod:
			dest.value = dest.value % src.value
		else:
			dest.value = dest.value // src.value
		return
	
	if isMul and src == state.rax:
		dest, src = src, dest
	
	if src != state.rdx and state.rdx.active:
		stack = saveReg(state, state.rdx)
		state.moveOperand(state.rdx, stack)
	
	if dest != state.rax:
		if state.rax.active:
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
	state.pushOperand(state.rdx if isMod else dest)

def mul(state, ir):
	mulDivMod(state, ir, True, False)

def div(state, ir):
	mulDivMod(state, ir, False, False)

def mod(state, ir):
	mulDivMod(state, ir, False, True)

def cmp(state, ir):
	if state.rcx.active:
		stack = saveReg(state, state.rcx)
		state.moveOperand(state.rcx, stack)
	state.rcx.type = I8
	
	r = state.getOperand(0)
	l = state.getOperand(1)
	
	if l.storage == Storage.IMM and r.storage == Storage.IMM:
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
	
	if l.storage == Storage.IMM or l.storage == Storage.STACK and r.storage == Storage.STACK:
		if state.rax.active:
			stack = saveReg(state, state.rax)
			state.moveOperand(state.rax, stack)
		state.rax.type = l.type
		state.appendInstr('mov', 
			Operand(state.rax, Usage.DEST), 
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

def fadd(state, ir):
	faddSubDivMul(state, ir, 'add', True)

def fsub(state, ir):
	faddSubDivMul(state, ir, 'sub', False)

def fmul(state, ir):
	faddSubDivMul(state, ir, 'mul', True)

def fdiv(state, ir):
	faddSubDivMul(state, ir, 'div', False)

def faddSubDivMul(state, ir, op, isCommutative):
	src = state.getOperand(0)
	dest = state.getOperand(1)
	
	if isCommutative and dest.storage != Storage.XMM and src.storage == Storage.XMM:
		src, dest = dest, src
	elif dest.storage != Storage.XMM:
		reg = state.findXmmReg(dest.type)
		if reg == None:
			reg = state.xmm8
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
			reg.type = dest.type
		moveData(state, dest, reg)
		dest = reg
	
	if src.storage == Storage.REG:
		reg = state.findXmmReg(dest.type)
		if reg == None:
			reg = state.xmm9
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
			reg.type = src.type
		moveData(state, src, reg)
		src = reg
	
	state.appendInstr('{}{}'.format(op, 'sd' if dest.type == F64 else 'ss'),
			Operand(dest, Usage.DEST), 
			Operand(src, Usage.SRC))
	
	state.popOperand()
	state.popOperand()
	state.pushOperand(dest)

def fcmp(state, ir):
	if state.rcx.active:
		stack = saveReg(state, state.rcx)
		state.moveOperand(state.rcx, stack)
	state.rcx.type = I8
	
	r = state.getOperand(0)
	l = state.getOperand(1)
	
	if type(ir) == FEq:
		opcode = 'sete'
	elif type(ir) == FNEq:
		opcode = 'setne'
	elif type(ir) == FLess:
		opcode = 'setl'
	elif type(ir) == FLessEq:
		opcode = 'setle'
	elif type(ir) == FGreater:
		opcode = 'setg'
	elif type(ir) == FGreaterEq:
		opcode = 'setge'
	else:
		assert 0
	
	if l.storage != Storage.XMM:
		reg = state.findXmmReg(l.type)
		if reg == None:
			reg = state.xmm8
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
			reg.type = l.type
		moveData(state, l, reg)
		l = reg
	
	if r.storage == Storage.REG:
		reg = state.findXmmReg(dest.type)
		if reg == None:
			reg = state.xmm9
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
			reg.type = r.type
		moveData(state, r, reg)
		r = reg
	
	state.appendInstr('xor', 
		Operand(state.rcx, Usage.DEST, I64), 
		Operand(state.rcx, Usage.DEST, I64))
	state.appendInstr('comis{}'.format('d' if l.type == F64 else 's'), 
		Operand(l, Usage.DEST), 
		Operand(r, Usage.SRC))
	state.appendInstr(opcode, 
		Operand(state.rcx, Usage.DEST))

	state.popOperand()
	state.popOperand()
	state.pushOperand(state.rcx)

class AMD64_CLASS(Enum):
	INTEGER     = "INTEGER"
	SSE         = "SSE"
	# SSEUP       = "SSEUP"
	# X87         = "X87"
	# X87UP       = "X87UP"
	# COMPLEX_X87 = "COMPLEX_X87"
	MEMORY      = "MEMORY"		

def classifyComposite(t):
	# larger structs are passed via registers only if using larger SSE regs, which we don't support yet
	# if t.byteSize > 64 or not t.aligned:
	if t.byteSize > 16 or not t.aligned:
		return [AMD64_CLASS.MEMORY]
	
	result = []
	c = None
	byteCt = 0
	lastOffset = 0
	for (offset, f) in t.types:
		assert not f.isCompositeType
		if offset > lastOffset:
			byteCt += f.byteSize
		else:
			byteCt = max(byteCt, f.byteSize)
		lastOffset = offset
		c2 = classify(f)[0]
		if c == c2:
			pass
		elif AMD64_CLASS.MEMORY in (c, c2):
			c = AMD64_CLASS.MEMORY
		elif AMD64_CLASS.INTEGER in (c, c2):
			c = AMD64_CLASS.INTEGER
		else:
			c = AMD64_CLASS.SSE
		
		if byteCt >= 8:
			assert byteCt == 8 # always true if fields are aligned
			assert c != AMD64_CLASS.MEMORY
			# if c == AMD64_CLASS.MEMORY:
				# return [AMD64_CLASS.MEMORY]
			result.append(c)
			byteCt = 0
			c = None
	
	if c:
		result.append(c)
	
	return result

def classify(t):
	if t.isFloatType:
		assert t.byteSize <= 8
		return [AMD64_CLASS.SSE]
	elif t.byteSize <= 8 and not t.isCompositeType:
		return [AMD64_CLASS.INTEGER]
	else:
		assert t.isCompositeType
		return classifyComposite(t)

def assignTargets(state, types, ret=False, memStructReturn=False):
	classifiers = []
	for t in types:
		classifiers.append(classify(t))
	
	if ret:
		intArgRegs = list(reversed([state.rax, state.rdx]))
		xmmArgRegs = list(reversed([state.xmm0, state.xmm1]))
	else:
		intArgRegs = list(reversed(state.intArgRegs))
		xmmArgRegs = list(reversed(state.xmmArgRegs))
		if memStructReturn:
			intArgRegs.pop()
	
	assignments = []
	for c in classifiers:
		if c[0] == AMD64_CLASS.MEMORY:
			assignments.append(None)
			continue
		
		intCt = 0
		xmmCt = 0
		for ec in c:
			if ec == AMD64_CLASS.INTEGER:
				intCt += 1
			else:
				xmmCt += 1
		
		if len(xmmArgRegs) < xmmCt or len(intArgRegs) < intCt:
			assignments.append(None)
			continue
		
		assignment = []
		for (i, ec) in enumerate(c):
			if ec == AMD64_CLASS.INTEGER:
				assignment.append(intArgRegs.pop())
			else:
				assignment.append(xmmArgRegs.pop())
		assignments.append(assignment)
	
	return assignments

def call(state, ir):
	for reg in state.intRegs:
		if reg.calleeSaved:
			continue
		if reg.active:
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
	for reg in state.xmmRegs:
		if reg.active:
			stack = saveReg(state, reg)
			state.moveOperand(reg, stack)
	
	retRegs = None
	retStackTarget = None
	if ir.retType:
		retRegs = assignTargets(state, [ir.retType], ret=True)[0]
		if retRegs == None:
			retStackTarget = state.findTarget(ir.retType, True)
			retStackTarget.setActive(True)
			state.appendInstr('lea', 
				Operand(state.rdi, Usage.DEST, I64), 
				Operand(retStackTarget, Usage.ADDR))
	
	funcAddr = state.getOperand(0)
	
	args = []
	for i in reversed(range(1, ir.argCt + 1)):
		src = state.getOperand(i)
		args.append(src)
	
	xmmCount = 0
	regAssignments = assignTargets(state, [arg.type for arg in args], memStructReturn=retStackTarget != None)
	stackArgs = []
	requiredStackSpace = 0
	for (arg, regs) in zip(args, regAssignments):
		if regs == None:
			stackArgs.append(arg)
			count = arg.type.byteSize // 8
			if arg.type.byteSize % 8 != 0:
				count += 1
			requiredStackSpace += count
			continue
		
		for (i, reg) in enumerate(regs):
			if type(reg) == XmmTarget:
				xmmCount += 1
			t = I64 if arg.type.byteSize > 8 else arg.type
			moveData(state, arg, reg, srcOffset=ImmTarget(I64, i*8) if i else None, type=t)
	
	if len(stackArgs) > 0:
		for stackSlot in reversed(state.callStack):
			if stackSlot.active:
				break
			else:
				requiredStackSpace -= 1
		
		state.allocateStack(requiredStackSpace)
		
		offset = 0
		for src in stackArgs:
			moveData(state, src, StackTarget(src.type, offset, fromTop=True))
			offset += src.type.byteSize
			if offset % 8 != 0:
				offset += offset % 8
	
	if ir.cVarArgs:
		state.appendInstr('mov', 
			Operand(state.rax, Usage.DEST, I8), 
			Operand(ImmTarget(I8, xmmCount), Usage.SRC, I8))
	
	state.appendInstr('call', Operand(funcAddr, Usage.SRC))
	
	for _ in range(0, ir.argCt + 1):
		state.popOperand()
	
	if ir.retType:
		if retStackTarget:
			retStackTarget.setActive(False)
			state.pushOperand(retStackTarget)
		elif len(retRegs) == 1:
			retRegs[0].type = ir.retType
			state.pushOperand(retRegs[0])
		elif len(retRegs) == 2:
			stack = state.findTarget(ir.retType, True)
			moveData(state, retRegs[0], stack, type=I64)
			moveData(state, retRegs[1], stack, type=I64, destOffset=ImmTarget(I64, 8))
			stack.type = ir.retType
			state.pushOperand(stack)
		else:
			assert 0
			

def brOrBrIf(state, ir, nextIR, isBrIf):
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
		
		state.appendInstr('jz {}'.format(state.fnIR.blockDefs[ir.elseIndex].label))
	
	if type(nextIR) != BlockMarker or nextIR.index != ir.index:
		state.appendInstr('jmp {}'.format(state.fnIR.blockDefs[ir.index].label))
	
	for _ in outputTargets:
		state.popOperand()

def br(state, ir, nextIR):
	brOrBrIf(state, ir, nextIR, False)

def brIf(state, ir, nextIR):
	brOrBrIf(state, ir, nextIR, True)
	
def ret(state, ir):
	if state.fnIR.retType:
		retTargets = assignTargets(state, [state.fnIR.retType], ret=True)[0]
		src = state.getOperand(0)
		if retTargets == None:
			moveData(state, src, state.operandStack[0], destDeref=True)
			moveData(state, state.operandStack[0], state.rax)
		else:
			moveData(state, src, retTargets[0], type=I64)
			if len(retTargets) > 1:
				assert len(retTargets) == 2 and src.storage == Storage.STACK
				moveData(state, src, retTargets[1], type=I64, srcOffset=ImmTarget(I64, 8))
	
	state.appendInstr('jmp {}__ret'.format(state.fnIR.name))

def saveReg(state, reg, exclude=[]):
	assert reg.active
	
	stack = state.findTarget(reg.type, True, exclude=exclude)
	state.appendInstr('movq' if reg.storage == Storage.XMM else 'mov', 
		Operand(stack, Usage.DEST), 
		Operand(reg, Usage.SRC))
	
	return stack

def restoreReg(state, stack, reg):
	reg.type = stack.type
	state.appendInstr('mov', 
		Operand(reg, Usage.DEST), 
		Operand(stack, Usage.SRC))

def delcareExterns(mod, output):
	for decl in mod.mods:
		delcareExterns(decl, output)
	
	for decl in mod.fns:
		if decl.extern:
			output.write('extern {}\n'.format(decl.mangledName))

def declareFns(mod, output):
	for decl in mod.mods:
		declareFns(decl, output)
	
	for decl in mod.fns:
		if not decl.extern:
			output.write('global {}\n'.format(decl.mangledName))

def defineStatics(mod, output):
	for decl in mod.mods:
		defineStatics(decl, output)
	
	for decl in mod.fns:
		if decl.extern:
			continue
		
		for staticDef in decl.ir.staticDefs:
			bytes = ','.join(str(b) for b in staticDef.toBytes())
			output.write('\t{}: db {}\n'.format(staticDef.label, bytes))
	
	for decl in mod.statics:
		if decl.extern:
			continue
		
		bytes = ','.join(str(b) for b in decl.staticValue.toBytes())
		output.write('\t{}: db {}\n'.format(decl.staticValue.label, bytes))
	
	if mod.isImpl and mod.vtbl:
		output.write('\t{}:\n'.format(mod.vtblName))
		for name in mod.vtbl:
			output.write('\t\tdq {}\n'.format(name))

def defineFns(mod, output):
	for decl in mod.mods:
		defineFns(decl, output)
	
	for decl in mod.fns:
		if decl.extern:
			continue
		
		state = GeneratorState(decl.ir)
		state.omitFramePointer = False
		state.generateAsm(output)

import sys

class OutputWriter:
	def __init__(self):
		self.output = StringIO()
	
	def write(self, s):
		# sys.stdout.write(s)
		self.output.write(s)
	
	def getvalue(self):
		return self.output.getvalue()



macOSStartup = '''
	_caesar_start:
		sub rsp, 8
		call {}
		mov rax, 0x02000001
		mov rdi, 0
		syscall
'''

linuxStartup = '''
	main:
		endbr64
		sub rsp, 8
		call {}
		hlt
		ret
'''

def generateAsm(mod):
	output = OutputWriter()
	
	if mod.mainFn and platform.Linux:
		output.write('\nextern _start\n')
		output.write('\nextern __libc_start_main\n')
	
	delcareExterns(mod, output)
	
	if mod.mainFn:
		if platform.Linux:
			output.write('\nglobal main\n')
		else:
			output.write('\nglobal _caesar_start\n')
	
	declareFns(mod, output)
	
	output.write('\nsection .text\n')
	
	if mod.mainFn:
		if platform.MacOS:
			startup = macOSStartup
		elif platform.Linux:
			startup = linuxStartup
		else:
			assert 0
		
		output.write(startup.format(mod.mainFn.mangledName))
	
	defineFns(mod, output)
	
	output.write('\nsection .data\n')
	defineStatics(mod, output)
	
	return output.getvalue()

