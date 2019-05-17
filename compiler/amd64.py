from .ir                                          import Move, Add, Call, Cmp, Div, Ret, IfElse, \
	                                                     While, InfixOp, Sub, Mul, Storage


ARG_REGS = ['rdi', 'rsi', 'rdx', 'rcx', 'r8', 'r9']

def targetToOperand(target, fnIR, offset=0):
	if target.storage == Storage.LOCAL:
		return '[rsp + {}]'.format((fnIR.sp - target.value)*8 + offset)
	elif target.storage == Storage.STATIC:
		return '{}__static__{}'.format(fnIR.name, target.value)
	elif target.storage == Storage.IMM:
		return str(target.value)
	elif target.value in fnIR.paramNames:
		return '[rsp + {}] ; param: {}'.format((fnIR.sp - fnIR.paramNames.index(target.value))*8 + offset, target.value)
	elif target.value in fnIR.locals:
		return '[rsp + {}] ; local: {}'.format((fnIR.sp + offset - fnIR.locals[target.value])*8 + offset, target.value)
	else:
		return target.value

def irToAsm(instr, fnIR):
	if type(instr) == Move:
		lines = []
		lines.append('mov rax, {}'.format(targetToOperand(instr.src, fnIR)))
		
		for _ in range(0, instr.srcDeref):
			lines.append('mov rcx, [rax]\n\t\tmov rax, rcx')
		
		lines.append('mov {}, rax'.format(targetToOperand(instr.dest, fnIR)))
		return '\n\t\t'.join(lines)
	elif type(instr) == Call:
		lines = []
		for (i, arg) in enumerate(instr.args[0:len(ARG_REGS)]):
			lines.append('mov {}, {}'.format(ARG_REGS[i], targetToOperand(arg, fnIR)))
		stackArgs = instr.args[len(ARG_REGS):]
		size = 8*len(stackArgs)
		if size % 16 != 0:
			size += 8
		if len(stackArgs) > 0:
			lines.append('sub rsp, {}'.format(size))
			for (i, arg) in enumerate(stackArgs):
				lines.append('mov rax, {}'.format(targetToOperand(arg, fnIR, size)))
				lines.append('mov [rsp + {}], rax'.format(8*i))
		if instr.cVarArgs:
			lines.append('mov al, 0')
		lines.append('call {}'.format(instr.callee.value))
		if len(stackArgs) > 0:
			lines.append('add rsp, {}'.format(size))
		lines.append('mov {}, rax'.format(targetToOperand(instr.dest, fnIR)))
		return '\n\t\t'.join(lines)
	elif type(instr) == Ret:
		output = '' if instr.target == None else 'mov rax, {}\n\t\t'.format(targetToOperand(instr.target, fnIR))
		return '{}jmp {}__end'.format(output, fnIR.name)
	elif type(instr) == IfElse:
		elseLabel = '{}__elselabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		endLabel = '{}__endiflabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		
		ifRetVal = ''
		elseRetVal = ''
		if len(instr.ifBlock) > 0:
			ifRetVal = '\t\tmov rax, {} ; save if expr value\n'.format(
				targetToOperand(instr.ifBlock[-1].dest, fnIR))
		if instr.elseBlock and len(instr.elseBlock) > 0:
			elseRetVal = '\t\tmov rax, {} ; save else expr value\n'.format(
				targetToOperand(instr.elseBlock[-1].dest, fnIR))
		
		return 'mov rax, {}\n\t\tcmp rax, 0\n\t\tjz {}\n{}{}\t\tjmp {}\n\t{}:\n{}{}\t{}:\n\t\tmov {}, rax'.format(
			targetToOperand(instr.target, fnIR), 
			elseLabel, 
			irBlockToAsm(instr.ifBlock, fnIR), 
			ifRetVal, 
			endLabel, 
			elseLabel, 
			irBlockToAsm(instr.elseBlock, fnIR), 
			elseRetVal, 
			endLabel,
			targetToOperand(instr.dest, fnIR)
		)
	elif type(instr) == While:
		startLabel = '{}__whilelabel__{}'.format(fnIR.name, fnIR.labelsCount)
		endLabel = '{}__endwhilelabel__{}'.format(fnIR.name, fnIR.labelsCount)
		fnIR.labelsCount += 1
		result = '\n\t{}:\n{}\t\tmov rax, {}\n\t\tcmp rax, 0\n\t\tjz {}\n{}\t\tjmp {}\n\t{}:'.format(
			startLabel,
			irBlockToAsm(instr.condBlock, fnIR), 
			targetToOperand(instr.target, fnIR), 
			endLabel, 
			irBlockToAsm(instr.block, fnIR), 
			startLabel,
			endLabel
		)
		return result
	elif type(instr) == Cmp:
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
		return 'xor rcx, rcx\n\t\tmov rax, {}\n\t\tcmp rax, {}\n\t\t{} cl\n\t\tmov {}, rcx'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
			opcode,
			targetToOperand(instr.dest, fnIR))
	elif type(instr) == Add:
		return 'mov rax, {}\n\t\tadd rax, {}\n\t\tmov {}, rax'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR))
	elif type(instr) == Sub:
		return 'mov rax, {}\n\t\tsub rax, {}\n\t\tmov {}, rax'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR))
	elif type(instr) == Mul:
		return 'mov rax, {}\n\t\tmov rcx, {}\n\t\tmul rcx\n\t\tmov {}, rax'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR))
	elif type(instr) == Div:
		return 'mov rax, {}\n\t\tmov rcx, {}\n\t\tdiv rcx\n\t\tmov {}, rax'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR))
	else:
		assert 0

def irBlockToAsm(irBlock, fnIR):
	lines = []
	for instr in irBlock:
		lines.append('\t\t' + irToAsm(instr, fnIR) + '\n')
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

