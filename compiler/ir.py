from enum                            import Enum
from .parser                         import FnDeclAST, FnCallAST, ValueRefAST, StrLitAST, BlockAST, \
	                                        IntLitAST, ReturnAST, LetAST, IfAST, InfixOpAST, InfixOp, \
											CoercionAST, BoolLitAST, WhileAST, AsgnAST, DerefAST, \
											IndexOpAST, VoidAST
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

def indexToIR(scope, ind, fn, block):
	exprToIR(scope, ind.expr, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, ind.index, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Add(l, r, dest))
	
	fn.sp += 1
	
	src = block[-1].dest
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(Move(src, dest, 1))

def ifBlockToIR(scope, ifAst, fn, block):
	exprToIR(scope, ifAst.expr, fn, block)
	result = block[-1].dest.clone()
	
	ifBlock = []
	blockToIR(scope, ifAst.ifBlock.exprs, fn, ifBlock)
	
	elseBlock = []
	if ifAst.elseBlock != None:
		blockToIR(scope, ifAst.elseBlock.exprs, fn, elseBlock)
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, Type.I8, fn.sp)
	
	block.append(IfElse(result, dest, ifBlock, elseBlock))

def whileBlockToIR(scope, whileAst, fn, block):
	condBlock = []
	exprToIR(scope, whileAst.expr, fn, condBlock)
	result = condBlock[-1].dest.clone()
	
	whileBlock = []
	blockToIR(scope, whileAst.block.exprs, fn, whileBlock)
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
	elif type(expr) == IndexOpAST:
		indexToIR(scope, expr, fn, block)
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
	elif type(expr) == VoidAST:
		pass
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
	blockToIR(scope, fnDecl.body.exprs, fn, fn.block)
	return fn

def generateIR(mod):
	for decl in mod.modDecls:
		generateIR(decl)
	
	for decl in mod.fnDecls:
		if decl.extern:
			continue
		ir = fnToIR(mod, decl)
		mod.symbolTable[decl.name].ir = ir