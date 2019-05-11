from enum                            import Enum
from .parser                         import FnDeclAST, FnCallAST, ValueRefAST, StrLitAST, BlockAST, \
	                                        IntLitAST, ReturnAST, LetAST, IfAST, InfixOpAST, InfixOp, \
											CoercionAST, BoolLitAST

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
	def __init__(self, src, dest):
		self.src = src
		self.dest = dest
	
	def __str__(self):
		return 'move {} = {}'.format(self.dest, self.src)

class Call(Instr):
	def __init__(self, callee, args, dest):
		self.callee = callee
		self.args = args
		self.dest = dest
	
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
	def __init__(self, target, ifBlock, elseBlock):
		self.target = target
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return 'if {}\n{}\telse\n{}'.format(self.target, blockToStr(self.ifBlock, 2), blockToStr(self.elseBlock, 2))

class Eq(Instr):
	def __init__(self, l, r, dest):
		self.l = l
		self.r = r
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

def returnToIR(ret, fn, block):
	target = None
	if ret.expr != None:
		exprToIR(ret.expr, fn, block)
		target = block[-1].dest.clone()
	
	block.append(Ret(target))

def letToIR(let, fn, block):
	if let.expr == None:
		src = Target(Storage.IMM, Type.I8, 0)
	else:
		exprToIR(let.expr, fn, block)
		src = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	fn.locals[let.name] = fn.sp
	
	block.append(Move(src, dest))

def ifBlockToIR(ifAst, fn, block):
	exprToIR(ifAst.expr, fn, block)
	result = block[-1].dest.clone()
	
	ifBlock = []
	blockToIR(ifAst.ifBlock, fn, ifBlock)
	
	elseBlock = []
	if ifAst.elseBlock != None:
		blockToIR(ifAst.elseBlock, fn, elseBlock)
	
	block.append(IfElse(result, ifBlock, elseBlock))

def boolLitToIR(lit, fn, block):
	fn.sp += 1
	
	src = Target(Storage.IMM, Type.I8, 1 if lit.value else 0)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def intLitToIR(lit, fn, block):
	fn.sp += 1
	
	src = Target(Storage.IMM, Type.I8, lit.value)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def strLitToIR(lit, fn, block):
	addr = len(fn.staticDefs)
	fn.staticDefs.append(lit.value)
	fn.sp += 1
	
	src = Target(Storage.STATIC, Type.I8, addr)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def fnCallToIR(fnCall, fn, block):
	args = []
	for expr in fnCall.args:
		exprToIR(expr, fn, block)
		args.append(block[-1].dest.clone())
	
	if type(fnCall.expr) == ValueRefAST:
		src = Target(Storage.LABEL, Type.I8, fnCall.expr.name)
	else:
		exprToIR(fnCall.expr, fn, block)
		src = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Call(src, args, dest))

def valueRefToIR(expr, fn, block):
	fn.sp += 1
	
	src = Target(Storage.LABEL, Type.I8, expr.name)
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest))

def listToIr(exprs, fn, block):
	for expr in exprs:
		exprToIR(expr, fn, block)

def eqToIr(eq, fn, block):
	exprToIR(eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Eq(l, r, dest))

def addToIr(eq, fn, block):
	exprToIR(eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Add(l, r, dest))

def subToIr(eq, fn, block):
	exprToIR(eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Sub(l, r, dest))

def mulToIr(eq, fn, block):
	exprToIR(eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Mul(l, r, dest))

def divToIr(eq, fn, block):
	exprToIR(eq.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(eq.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Div(l, r, dest))

def exprToIR(expr, fn, block):
	if type(expr) == BoolLitAST:
		boolLitToIR(expr, fn, block)
	elif type(expr) == IntLitAST:
		intLitToIR(expr, fn, block)
	elif type(expr) == StrLitAST:
		strLitToIR(expr, fn, block)
	elif type(expr) == ValueRefAST:
		valueRefToIR(expr, fn, block)
	elif type(expr) == FnCallAST:
		fnCallToIR(expr, fn, block)
	elif type(expr) == LetAST:
		letToIR(expr, fn, block)
	elif type(expr) == IfAST:
		ifBlockToIR(expr, fn, block)
	elif type(expr) == ReturnAST:
		returnToIR(expr, fn, block)
	elif type(expr) == BlockAST:
		listToIr(expr.exprs, fn, block)
	elif type(expr) == CoercionAST:
		print('skipping coercion (unimplemented)')
	elif type(expr) == InfixOpAST and expr.op == InfixOp.EQ:
		eqToIr(expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.PLUS:
		addToIr(expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.MINUS:
		subToIr(expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.TIMES:
		mulToIr(expr, fn, block)
	elif type(expr) == InfixOpAST and expr.op == InfixOp.DIV:
		divToIr(expr, fn, block)
	else:
		assert 0

def blockToIR(astBlock, fn, block):
	for expr in astBlock:
		exprToIR(expr, fn, block)

def blockToStr(irBlock, indentLevel = 1):
	lines = ['\t' * (indentLevel - 1) + '{']
	lines.extend([('\t' * indentLevel) + str(instr) for instr in irBlock])
	lines.append('\t' * (indentLevel - 1) + '}\n')
	return '\n'.join(lines)

class FnIR:
	def __init__(self, name, paramNames):
		self.name = name
		self.paramNames = paramNames
		self.block = []
		self.sp = len(paramNames)
		self.staticDefs = []
		self.locals = {}
		self.labelsCount = 0
	
	def __str__(self):
		return self.name + '()\n' + blockToStr(self.block)

def fnToIR(fnDecl):
	fn = FnIR(fnDecl.mangledName, [param.name for param in fnDecl.params])
	blockToIR(fnDecl.body, fn, fn.block)
	return fn

def generateIR(mod):
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		ir = fnToIR(decl)
		mod.symbolTable[decl.name].ir = ir


ARG_REGS = ['rdi', 'rsi', 'rdx', 'rcx', 'r8', 'r9']

def targetToOperand(target, fnIR):
	if target.storage == Storage.LOCAL:
		return '[rsp + {}]'.format((fnIR.sp - target.value)*8)
	elif target.storage == Storage.STATIC:
		return '{}__static__{}'.format(fnIR.name, target.value)
	elif target.storage == Storage.IMM:
		return str(target.value)
	elif target.value in fnIR.paramNames:
		return '[rsp + {}] ; param: {}'.format((fnIR.sp - fnIR.paramNames.index(target.value))*8, target.value)
	elif target.value in fnIR.locals:
		return '[rsp + {}] ; local: {}'.format((fnIR.sp - fnIR.locals[target.value])*8, target.value)
	else:
		return target.value

def irToAsm(instr, fnIR, mod):
	if type(instr) == Move:
		return 'mov rax, {}\n\t\tmov {}, rax'.format(
			targetToOperand(instr.src, fnIR),
			targetToOperand(instr.dest, fnIR))
	elif type(instr) == Call:
		lines = []
		for (i, arg) in enumerate(instr.args[0:len(ARG_REGS)]):
			lines.append('mov {}, {}'.format(ARG_REGS[i], targetToOperand(arg, fnIR)))
		for arg in reversed(instr.args[len(ARG_REGS):]):
			lines.append('push qword {}'.format(targetToOperand(arg, fnIR)))
		if len(instr.args[len(ARG_REGS):]) % 2 != 0:
			lines.append('sub rsp, 8')
		lines.append('call {}'.format(mod.symbolTable[instr.callee.value].mangledName))
		for arg in instr.args[len(ARG_REGS):]:
			lines.append('pop qword rax')
		if len(instr.args[len(ARG_REGS):]) % 2 != 0:
			lines.append('add rsp, 8')
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
		return 'mov rax, {}\n\t\tcmp rax, 0\n\t\tjz {}\n{}\t\tjmp {}\n\t\t{}:\n{}\t\t{}:'.format(
			targetToOperand(instr.target, fnIR), 
			elseLabel, 
			irBlockToAsm(instr.ifBlock, fnIR, mod), 
			endLabel, 
			elseLabel, 
			irBlockToAsm(instr.elseBlock, fnIR, mod), 
			endLabel
		)
	elif type(instr) == Eq:
		return 'mov rbx, 0\n\t\tmov rax, {}\n\t\tcmp rax, {}\n\t\tsetz bl\n\t\tmov {}, rbx'.format(
			targetToOperand(instr.l, fnIR),
			targetToOperand(instr.r, fnIR),
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

def irBlockToAsm(irBlock, fnIR, mod):
	lines = []
	for instr in irBlock:
		lines.append('\t\t' + irToAsm(instr, fnIR, mod) + '\n')
	return ''.join(lines)

def generateAsm(mod):
	externSymbols = []
	fns = []
	#statics = []
	
	for decl in mod.fnDecls:
		if decl.extern:
			externSymbols.append(decl)
		else:
			fns.append(decl)
	
	lines = []
	for decl in externSymbols:
		lines.append('extern {}\n'.format(decl.mangledName))
	
	lines.append('\nglobal _start\n')
	for decl in fns:
		lines.append('global {}\n'.format(decl.mangledName))
	
	lines.append('\nsection .data\n')
	for decl in fns:
		for (i, d) in enumerate(decl.ir.staticDefs):
			lines.append('\t{}__static__{}: db {},0\n'.format(decl.ir.name, i, ','.join([str(b) for b in d])))
	
	lines.extend(
	[
		'\nsection .text\n',
		'\t_start:\n',
		'\t\tsub rsp, 8\n',
		'\t\tcall main\n',
		'\t\tmov rax, 0x02000001\n',
		'\t\tmov rdi, 0\n',
		'\t\tsyscall\n'
	])
	
	for decl in fns:
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
		
		lines.append(irBlockToAsm(decl.ir.block, decl.ir, mod))
		
		lines.append('\t\t{}__end:\n'.format(decl.ir.name))
		
		if stackSize > 0:
			lines.append('\t\tadd rsp, {}\n'.format(stackSize))
		
		lines.append('\t\tret\n')
	
	return ''.join(lines)

