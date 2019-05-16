from enum                            import Enum
from .parser                         import FnDeclAST, FnCallAST, ValueRefAST, StrLitAST, BlockAST, \
	                                        IntLitAST, ReturnAST, LetAST, IfAST, InfixOpAST, InfixOp, \
											CoercionAST, BoolLitAST, WhileAST, AsgnAST, DerefAST
from .analyzer                       import lookupSymbol

class Type(Enum):
	I1 = 'i1'
	I2 = 'i2'
	I4 = 'i4'
	I8 = 'i8'
	F4 = 'f4'
	F8 = 'f8'

class Storage(Enum):
	IMM = 'IMM'
	LABEL = 'LABEL'
	LOCAL = 'LOCAL'
	STATIC = 'STATIC'

class Target:
	def __init__(self, storage, type, value):
		self.storage = storage
		self.type = type
		self.value = value
	
	def clone(self):
		return Target(self.storage, self.type, self.value)
	
	def __repr__(self):
		return self.__str__()
	
	def __str__(self):
		prefix = '$' if self.storage == Storage.LOCAL else \
			'&' if self.storage == Storage.STATIC else \
			''
		
		return '{} {}{}'.format(self.type.value, prefix, self.value)

class Instr:
	def __str__(self):
		return 'Instr'
	
	def __repr__(self):
		return self.__str__()

class Move(Instr):
	def __init__(self, src, dest, srcDeref=0):
		self.src = src
		self.dest = dest
		self.srcDeref = srcDeref
	
	def __str__(self):
		return 'move {} = {}'.format(self.dest, self.src)

class Call(Instr):
	def __init__(self, callee, args, dest, cVarArgs):
		self.callee = callee
		self.args = args
		self.dest = dest
		self.cVarArgs = cVarArgs
	
	def __str__(self):
		return '{} = call {}({})'.format(
			self.dest, self.callee, ', '.join([str(arg) for arg in self.args]))

class Ret(Instr):
	def __init__(self, target):
		self.target = target
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return 'ret{}'.format(target)

class IfElse(Instr):
	def __init__(self, target, dest, ifBlock, elseBlock):
		self.target = target
		self.dest = dest
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return 'if {}\n{}\telse\n{}'.format(self.target, blockToStr(self.ifBlock, 2), blockToStr(self.elseBlock, 2))

class While(Instr):
	def __init__(self, target, condBlock, block):
		self.target = target
		self.condBlock = condBlock
		self.block = block
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return 'if {}\n{}\telse\n{}'.format(self.target, blockToStr(self.ifBlock, 2), blockToStr(self.elseBlock, 2))

class Cmp(Instr):
	def __init__(self, l, r, op, dest):
		self.l = l
		self.r = r
		self.op = op
		self.dest = dest
	
	def __str__(self):
		return '{} = {} == {}'.format(self.dest, self.l, self.r)

class Add(Instr):
	def __init__(self, l, r, dest):
		self.l = l
		self.r = r
		self.dest = dest
	
	def __str__(self):
		return '{} = {} + {}'.format(self.dest, self.l, self.r)

class Sub(Instr):
	def __init__(self, l, r, dest):
		self.l = l
		self.r = r
		self.dest = dest
	
	def __str__(self):
		return '{} = {} - {}'.format(self.dest, self.l, self.r)

class Mul(Instr):
	def __init__(self, l, r, dest):
		self.l = l
		self.r = r
		self.dest = dest
	
	def __str__(self):
		return '{} = {} * {}'.format(self.dest, self.l, self.r)

class Div(Instr):
	def __init__(self, l, r, dest):
		self.l = l
		self.r = r
		self.dest = dest
	
	def __str__(self):
		return '{} = {} / {}'.format(self.dest, self.l, self.r)

def returnToIR(scope, ret, fn, block):
	target = None
	if ret.expr != None:
		exprToIR(scope, ret.expr, fn, block)
		target = block[-1].dest.clone()
	
	block.append(Ret(target))

def letToIR(scope, let, fn, block):
	if let.expr == None:
		src = Target(Storage.IMM, Type.I8, 0)
	else:
		exprToIR(scope, let.expr, fn, block)
		src = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	fn.locals[let.name] = fn.sp
	
	block.append(Move(src, dest))

def asgnToIR(scope, asgn, fn, block):
	exprToIR(scope, asgn.rvalue, fn, block)
	src = block[-1].dest.clone()
	
	dest = Target(Storage.LOCAL, Type.I8, fn.locals[asgn.lvalue.name])
	
	block.append(Move(src, dest))

def ifBlockToIR(scope, ifAst, fn, block):
	exprToIR(scope, ifAst.expr, fn, block)
	result = block[-1].dest.clone()
	
	ifBlock = []
	blockToIR(scope, ifAst.ifBlock, fn, ifBlock)
	
	elseBlock = []
	if ifAst.elseBlock != None:
		blockToIR(scope, ifAst.elseBlock, fn, elseBlock)
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(IfElse(result, dest, ifBlock, elseBlock))

def whileBlockToIR(scope, whileAst, fn, block):
	condBlock = []
	exprToIR(scope, whileAst.expr, fn, condBlock)
	result = condBlock[-1].dest.clone()
	
	whileBlock = []
	blockToIR(scope, whileAst.block, fn, whileBlock)
	block.append(While(result, condBlock, whileBlock))

def boolLitToIR(scope, lit, fn, block):
	fn.sp += 1
	
	src = Target(Storage.IMM, Type.I8, 1 if lit.value else 0)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def intLitToIR(scope, lit, fn, block):
	fn.sp += 1
	
	src = Target(Storage.IMM, Type.I8, lit.value)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def strLitToIR(scope, lit, fn, block):
	addr = len(fn.staticDefs)
	fn.staticDefs.append(lit.value)
	fn.sp += 1
	
	src = Target(Storage.STATIC, Type.I8, addr)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def fnCallToIR(scope, fnCall, fn, block):
	args = []
	for expr in fnCall.args:
		exprToIR(scope, expr, fn, block)
		args.append(block[-1].dest.clone())
	
	if type(fnCall.expr) == ValueRefAST:
		src = Target(Storage.LABEL, Type.I8, lookupSymbol(None, scope, fnCall.expr, False).mangledName)
	else:
		exprToIR(scope, fnCall.expr, fn, block)
		src = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Call(src, args, dest, fnCall.expr.resolvedType.cVarArgs))

def derefToIR(scope, expr, fn, block):
	exprToIR(scope, expr.expr, fn, block)
	
	fn.sp += 1
	
	src = block[-1].dest
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest, expr.derefCount))

def valueRefToIR(scope, expr, fn, block):
	fn.sp += 1
	
	src = Target(Storage.LABEL, Type.I8, expr.name)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def listToIR(scope, exprs, fn, block):
	for expr in exprs:
		exprToIR(scope, expr, fn, block)

def addToIR(scope, eq, fn, block):
	exprToIR(scope, eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Add(l, r, dest))

def subToIR(scope, eq, fn, block):
	exprToIR(scope, eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Sub(l, r, dest))

def mulToIR(scope, eq, fn, block):
	exprToIR(scope, eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Mul(l, r, dest))

def divToIR(scope, eq, fn, block):
	exprToIR(scope, eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Div(l, r, dest))

def cmpToIR(scope, op, fn, block):
	exprToIR(scope, op.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, op.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Cmp(l, r, op.op, dest))

def exprToIR(scope, expr, fn, block):
	if type(expr) == BoolLitAST:
		boolLitToIR(scope, expr, fn, block)
	elif type(expr) == IntLitAST:
		intLitToIR(scope, expr, fn, block)
	elif type(expr) == StrLitAST:
		strLitToIR(scope, expr, fn, block)
	elif type(expr) == ValueRefAST:
		valueRefToIR(scope, expr, fn, block)
	elif type(expr) == DerefAST:
		derefToIR(scope, expr, fn, block)
	elif type(expr) == FnCallAST:
		fnCallToIR(scope, expr, fn, block)
	elif type(expr) == LetAST:
		letToIR(scope, expr, fn, block)
	elif type(expr) == IfAST:
		ifBlockToIR(scope, expr, fn, block)
	elif type(expr) == WhileAST:
		whileBlockToIR(scope, expr, fn, block)
	elif type(expr) == ReturnAST:
		returnToIR(scope, expr, fn, block)
	elif type(expr) == BlockAST:
		listToIR(scope, expr.exprs, fn, block)
	elif type(expr) == CoercionAST:
		print('skipping coercion (unimplemented)')
		exprToIR(scope, expr.expr, fn, block)
	elif type(expr) == AsgnAST:
		asgnToIR(scope, expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.PLUS:
		addToIR(scope, expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.MINUS:
		subToIR(scope, expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.TIMES:
		mulToIR(scope, expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.DIV:
		divToIR(scope, expr, fn, block)
	elif type(expr) == InfixOpAST and \
		(expr.op == InfixOp.GREATER or expr.op == InfixOp.GREATEREQ or \
		expr.op == InfixOp.LESS or expr.op == InfixOp.LESSEQ or \
		expr.op == InfixOp.EQ or expr.op == InfixOp.NEQ):
		cmpToIR(scope, expr, fn, block)
	else:
		assert 0

def blockToIR(scope, astBlock, fn, block):
	for expr in astBlock:
		exprToIR(scope, expr, fn, block)

def blockToStr(irBlock, indentLevel = 1):
	lines = ['\t' * (indentLevel - 1) + '{']
	lines.extend([('\t' * indentLevel) + str(instr) for instr in irBlock])
	lines.append('\t' * (indentLevel - 1) + '}\n')
	return '\n'.join(lines)

class FnIR:
	def __init__(self, name, paramNames, cVarArgs):
		self.name = name
		self.paramNames = paramNames
		self.cVarArgs = cVarArgs
		self.block = []
		self.sp = len(paramNames)
		self.staticDefs = []
		self.locals = {}
		self.labelsCount = 0
	
	def __str__(self):
		return self.name + '()\n' + blockToStr(self.block)

def fnToIR(scope, fnDecl):
	fn = FnIR(fnDecl.mangledName, [param.name for param in fnDecl.params], fnDecl.cVarArgs)
	blockToIR(scope, fnDecl.body, fn, fn.block)
	return fn

def generateIR(mod):
	for decl in mod.modDecls:
		generateIR(decl)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		ir = fnToIR(mod, decl)
		mod.symbolTable[decl.name].ir = ir


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
			lines.append('mov rbx, [rax]\n\t\tmov rax, rbx')
		
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
		return 'mov rbx, 0\n\t\tmov rax, {}\n\t\tcmp rax, {}\n\t\t{} bl\n\t\tmov {}, rbx'.format(
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
		return 'mov rax, {}\n\t\tmov rbx, {}\n\t\tmul rbx\n\t\tmov {}, rax'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
			targetToOperand(instr.dest, fnIR))
	elif type(instr) == Div:
		return 'mov rax, {}\n\t\tmov rbx, {}\n\t\tdiv rbx\n\t\tmov {}, rax'.format(
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
				raise RuntimeError('unimplemented!')#lines.append('\t\tpush {}'.format(targetToOperand(arg, fnIR)))
		
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

