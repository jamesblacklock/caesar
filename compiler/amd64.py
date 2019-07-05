import ctypes
from .ir                                         import Move, Addr, Add, Call, Cmp, Div, Ret, IfElse, Break, \
	                                                     While, Loop, InfixOp, Sub, Mul, Coerce, Cont, Storage, \
                                                         I8, I16, I32, I64, U8, U16, U32, U64, IPTR, F32, F64


ARG_REGS = ['rdi', 'rsi', 'rdx', 'rcx', 'r8', 'r9']

class LoopInfo:
	def __init__(self, startLabel, endLabel, parent):
		self.startLabel = startLabel
		self.endLabel = endLabel
		self.parent = parent

def targetToOperand(target, fnIR, offset=0):
	byteSize = target.type.byteSize
	isFloat = target.type.isFloatType
	
	size = ''#None
	# if byteSize == 1:
	# 	size = 'byte '
	# elif byteSize == 2:
	# 	size = 'word '
	# elif byteSize == 4:
	# 	size = 'dword '
	# elif byteSize == 8:
	# 	size = 'qword '
	# else:
	# 	assert 0
	
	if target.storage == Storage.LOCAL:
		return '{}[rsp + {}]'.format(size, (fnIR.sp - target.value)*8 + offset)
	elif target.storage == Storage.STATIC:
		return '{}{}__static__{}'.format(size, fnIR.name, target.value)
	elif target.storage == Storage.IMM:
		value = target.value
		if target.type.isFloatType:
			if target.type.byteSize == 4:
				value = hex(ctypes.c_uint32.from_buffer(ctypes.c_float(value)).value)
			else:
				value = hex(ctypes.c_uint64.from_buffer(ctypes.c_double(value)).value)
		return '{}{}'.format(size, value)
	elif target.value in fnIR.paramNames:
		return '{}[rsp + {}] ; param: {}'.format(size, (fnIR.sp - fnIR.paramNames.index(target.value))*8 + offset, target.value)
	elif target.value in fnIR.locals:
		return '{}[rsp + {}] ; local: {}'.format(size, (fnIR.sp + offset - fnIR.locals[target.value])*8 + offset, target.value)
	else:
		return target.value

def getReg(type, ind=0):
	intReg = ['a', 'c', 'd']
	
	if type == I8 or type == U8:
		return '{}l'.format(intReg[ind])
	elif type == I16 or type == U16:
		return '{}x'.format(intReg[ind])
	elif type == I32 or type == U32:
		return 'e{}x'.format(intReg[ind])
	elif type == I64 or type == U64:
		return 'r{}x'.format(intReg[ind])
	elif type == F32 or type == F64:
		return 'xmm{}'.format(ind)
	else:
		assert 0

def irToAsm(instr, fnIR, loopInfo=None):
	if type(instr) == Move:
		reg = getReg(instr.dest.type)
		lines = []
		
		if not instr.dest.type.isFloatType:
			if instr.srcDeref:
				lines.append('lea rax, {}'.format(targetToOperand(instr.src, fnIR)))
				for _ in range(0, instr.srcDeref):
					lines.append('mov rcx, [rax]\n\t\tmov rax, rcx')
				
				lines.append('mov {}, [rax]'.format(reg))
			else:
				lines.append('mov {}, {}'.format(reg, targetToOperand(instr.src, fnIR, instr.src.offset)))
			
			lines.append('lea rdx, {}'.format(targetToOperand(instr.dest, fnIR)))
			for _ in range(0, instr.destDeref):
				lines.append('mov rcx, [rdx]\n\t\tmov rdx, rcx')
			
			lines.append('mov [rdx], {}'.format(reg))
		else:
			assert 0
			# src = targetToOperand(instr.src, fnIR)
			# opcode = 'movss' if instr.src.type.byteSize == 4 else 'movsd'
			
			# if instr.srcDeref:
			# 	lines.append('mov rax, {}'.format(reg, src))
				
			# 	for _ in range(0, instr.srcDeref-1):
			# 		lines.append('mov rcx, [rax]\n\t\tmov rax, rcx')
				
			# 	src = '[rax]'
			
			# lines.append('{} {}, {}'.format(opcode, reg, src))
			# lines.append('{} {}, {}'.format(opcode, targetToOperand(instr.dest, fnIR), reg))
		
		return '\n\t\t'.join(lines)
	elif type(instr) == Addr:
		reg = getReg(IPTR)
		lines = []
		lines.append('lea {}, {}'.format(reg, targetToOperand(instr.src, fnIR)))
		lines.append('mov {}, {}'.format(targetToOperand(instr.dest, fnIR), reg))
		return '\n\t\t'.join(lines)
	elif type(instr) == Call:
		lines = []
		for (i, arg) in enumerate(instr.args[0:len(ARG_REGS)]):
			regType = I64 if arg.type.isSigned else U64
			lines.append(coerce(arg, regType, fnIR))
			lines.append('mov {}, {}'.format(ARG_REGS[i], getReg(regType)))
		stackArgs = instr.args[len(ARG_REGS):]
		size = 8*len(stackArgs)
		if size % 16 != 0:
			size += 8
		if len(stackArgs) > 0:
			lines.append('sub rsp, {}'.format(size))
			for (i, arg) in enumerate(stackArgs):
				regType = I64 if arg.type.isSigned else U64
				lines.append(coerce(arg, regType, fnIR, size))
				lines.append('mov [rsp + {}], {}'.format(8*i, getReg(regType)))
		if instr.cVarArgs:
			lines.append('mov al, 0')
		lines.append('call {}'.format(instr.callee.value))
		if len(stackArgs) > 0:
			lines.append('add rsp, {}'.format(size))
		if instr.dest:
			lines.append('mov {}, {}'.format(targetToOperand(instr.dest, fnIR), getReg(instr.dest.type)))
		return '\n\t\t'.join(lines)
	elif type(instr) == Ret:
		output = '' if instr.target == None else 'mov {}, {}\n\t\t'.format(
			getReg(instr.target.type), targetToOperand(instr.target, fnIR))
		return '{}jmp {}__end'.format(output, fnIR.name)
	elif type(instr) == IfElse:
		reg = getReg(instr.target.type)
		elseLabel = '{}__elselabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		endLabel = '{}__endiflabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		
		ifRetVal = ''
		elseRetVal = ''
		destReg = None
		if instr.dest:
			destReg = getReg(instr.dest.type)
			if len(instr.ifBlock) > 0 and instr.ifBlock[-1].producesValue:
				ifRetVal = '\t\tmov {}, {} ; save if expr value\n'.format(
					destReg, targetToOperand(instr.ifBlock[-1].dest, fnIR))
			if instr.elseBlock and len(instr.elseBlock) > 0 and instr.elseBlock[-1].producesValue:
				elseRetVal = '\t\tmov {}, {} ; save else expr value\n'.format(
					destReg, targetToOperand(instr.elseBlock[-1].dest, fnIR))
		
		lines = []
		lines.append('mov {}, {}'.format(reg, targetToOperand(instr.target, fnIR)))
		lines.append('cmp {}, 0'.format(reg))
		lines.append('jz {}'.format(elseLabel))
		lines.append('{}{}'.format(irBlockToAsm(instr.ifBlock, fnIR, loopInfo), ifRetVal))
		lines.append('jmp {}'.format(endLabel))
		lines.append('{}:\n{}{}\t{}:'.format(elseLabel, irBlockToAsm(instr.elseBlock, fnIR, loopInfo), elseRetVal, endLabel))
		if instr.dest:
			lines.append('mov {}, {}'.format(targetToOperand(instr.dest, fnIR), destReg))
		return '\n\t\t'.join(lines)
	elif type(instr) == Loop:
		startLabel = '{}__looplabel__{}'.format(fnIR.name, fnIR.labelsCount)
		endLabel = '{}__endlooplabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		result = '\n\t{}:\n{}\t\tjmp {}\n\t{}:'.format(
			startLabel, 
			irBlockToAsm(instr.block, fnIR, LoopInfo(startLabel, endLabel, loopInfo)), 
			startLabel, 
			endLabel
		)
		return result
	elif type(instr) == While:
		reg = getReg(instr.target.type)
		startLabel = '{}__whilelabel__{}'.format(fnIR.name, fnIR.labelsCount)
		endLabel = '{}__endwhilelabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		result = '\n\t{}:\n{}\t\tmov {}, {}\n\t\tcmp {}, 0\n\t\tjz {}\n{}\t\tjmp {}\n\t{}:'.format(
			startLabel, 
			irBlockToAsm(instr.condBlock, fnIR, loopInfo), 
			reg, 
			targetToOperand(instr.target, fnIR), 
			reg, 
			endLabel, 
			irBlockToAsm(instr.block, fnIR, LoopInfo(startLabel, endLabel, loopInfo)), 
			startLabel, 
			endLabel
		)
		return result
	elif type(instr) == Break:
		if loopInfo == None:
			assert 0
		return 'jmp {}'.format(loopInfo.endLabel)
	elif type(instr) == Cont:
		if loopInfo == None:
			assert 0
		return 'jmp {}'.format(loopInfo.startLabel)
	elif type(instr) == Cmp:
		reg = getReg(instr.l.type)
		reg2 = getReg(instr.l.type, 1)
		opcode = 'setz'
		if instr.op == InfixOp.NEQ:
			opcode = 'setnz'
		elif instr.op == InfixOp.LESS:
			opcode = 'setl'
		elif instr.op == InfixOp.LESSEQ:
			opcode = 'setle'
		elif instr.op == InfixOp.GREATER:
			opcode = 'setg'
		elif instr.op == InfixOp.GREATEREQ:
			opcode = 'setge'
		return 'xor {}, {}\n\t\tmov {}, {}\n\t\tcmp {}, {}\n\t\t{} cl\n\t\tmov {}, {}'.format(
			reg2,
			reg2,
			reg,
			targetToOperand(instr.l, fnIR),
			reg,
			targetToOperand(instr.r, fnIR),
			opcode,
			targetToOperand(instr.dest, fnIR),
			reg2)
	elif type(instr) == Add:
		reg = getReg(instr.l.type)
		return 'mov {}, {}\n\t\tadd {}, {}\n\t\tmov {}, {}'.format(
			reg,
			targetToOperand(instr.l, fnIR),
			reg,
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR),
			reg)
	elif type(instr) == Sub:
		reg = getReg(instr.l.type)
		return 'mov {}, {}\n\t\tsub {}, {}\n\t\tmov {}, {}'.format(
			reg,
			targetToOperand(instr.l, fnIR),
			reg,
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR),
			reg)
	elif type(instr) == Mul:
		reg = getReg(instr.l.type)
		reg2 = getReg(instr.l.type, 1)
		return 'mov {}, {}\n\t\tmov {}, {}\n\t\tmul {}\n\t\tmov {}, {}'.format(
			reg,
			targetToOperand(instr.l, fnIR),
			reg2,
			targetToOperand(instr.r, fnIR),
			reg2,
			targetToOperand(instr.dest, fnIR),
			reg)
	elif type(instr) == Div:
		reg = getReg(instr.l.type)
		reg2 = getReg(instr.l.type, 1)
		return 'mov {}, {}\n\t\tmov {}, {}\n\t\tdiv {}\n\t\tmov {}, {}'.format(
			reg,
			targetToOperand(instr.l, fnIR),
			reg2,
			targetToOperand(instr.r, fnIR),
			reg2,
			targetToOperand(instr.dest, fnIR),
			reg)
	elif type(instr) == Coerce:
		lines = []
		lines.append(coerce(instr.src, instr.dest.type, fnIR))
		lines.append('mov {}, {}'.format(targetToOperand(instr.dest, fnIR), getReg(instr.dest.type)))
		return '\n\t\t'.join(lines)
	else:
		assert 0

def coerce(src, destType, fnIR, offset=0, regInd=0):
	if src.type.isFloatType == False and destType.isFloatType == False:
		if src.type.byteSize < destType.byteSize:
			if src.type.byteSize == 4:
				reg = getReg(I64 if src.type.isSigned else I32, regInd)
				opcode = 'movsxd' if src.type.isSigned else 'mov'
				return '{} {}, dword {}'.format(
					opcode, 
					reg, 
					targetToOperand(src, fnIR, offset))
			else:
				reg = getReg(destType, regInd)
				opcode = 'movsx' if src.type.isSigned else 'movzx'
				size = 'byte' if src.type.byteSize == 1 else 'word'
				return '{} {}, {} {}'.format(
					opcode, 
					reg, 
					size, 
					targetToOperand(src, fnIR, offset))
		else:
			reg = getReg(src.type, regInd)
			return 'mov {}, {}'.format(reg, targetToOperand(src, fnIR, offset))
	else:
		raise RuntimeError('unimplemented!')

def irBlockToAsm(irBlock, fnIR, loopInfo=None):
	lines = []
	for instr in irBlock:
		lines.append('\t\t\n\t\t; {}\n\t\t'.format(str(instr).split('\n')[0]) + irToAsm(instr, fnIR, loopInfo) + '\n')
	return ''.join(lines)

def delcareExterns(mod, lines):	
	for decl in mod.modDecls:
		delcareExterns(decl, lines)
	
	for decl in mod.fnDecls:
		if decl.extern:
			lines.append('extern {}\n'.format(decl.mangledName))

def declareFns(mod, lines):
	for decl in mod.modDecls:
		declareFns(decl, lines)
	
	for decl in mod.fnDecls:
		if not decl.extern:
			lines.append('global {}\n'.format(decl.mangledName))

def defineStatics(mod, lines):
	for decl in mod.modDecls:
		defineStatics(decl, lines)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		
		for (i, d) in enumerate(decl.ir.staticDefs):
			lines.append('\t{}__static__{}: db {},0\n'.format(decl.ir.name, i, ','.join([str(b) for b in d])))

def defineFns(mod, lines):
	for decl in mod.modDecls:
		defineFns(decl, lines)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		
		lines.append('\t{}:\n'.format(decl.ir.name))
		stackSize = decl.ir.sp * 8
		if stackSize % 16 == 0:
			stackSize += 8
		if stackSize > 0:
			lines.append('\t\tsub rsp, {}\n'.format(stackSize))
		
		for (i, param) in enumerate(decl.ir.paramNames):
			if i < len(ARG_REGS):
				lines.append('\t\tmov [rsp + {}], {} ; param: {}\n'.format(
					(decl.ir.sp - i)*8, ARG_REGS[i], param))
			else:
				# the `+ 2` points back into the previous stack frame, behind the return address
				stackOffset = i - len(ARG_REGS) + 2
				lines.append('\t\tmov rax, [rsp + {}]\n'.format(
					(decl.ir.sp + stackOffset)*8))
				lines.append('\t\tmov [rsp + {}], rax ; param: {}\n'.format(
					(decl.ir.sp - i)*8, param))
		
		lines.append(irBlockToAsm(decl.ir.block, decl.ir))
		
		lines.append('\t{}__end:\n'.format(decl.ir.name))
		
		if stackSize > 0:
			lines.append('\t\tadd rsp, {}\n'.format(stackSize))
		
		lines.append('\t\tret\n')

def generateAsm(mod):
	lines = []
	delcareExterns(mod, lines)
	
	if 'main' in mod.symbolTable:
		lines.append('\nglobal _start\n')
	
	declareFns(mod, lines)
	
	lines.append('\nsection .data\n')
	defineStatics(mod, lines)
	
	lines.append('\nsection .text\n')
	
	if 'main' in mod.symbolTable:
		lines.extend(
		[
			'\t_start:\n',
			'\t\tsub rsp, 8\n',
			'\t\tcall {}\n'.format(mod.symbolTable['main'].mangledName),
			'\t\tmov rax, 0x02000001\n',
			'\t\tmov rdi, 0\n',
			'\t\tsyscall\n'
		])
	
	defineFns(mod, lines)
	
	return ''.join(lines)

