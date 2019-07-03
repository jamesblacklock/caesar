from enum                            import Enum
from .ast                            import FnDeclAST, FnCallAST, ValueRefAST, StrLitAST, BlockAST, \
	                                        IntLitAST, ReturnAST, LetAST, IfAST, InfixOpAST, InfixOp, \
											CoercionAST, BoolLitAST, WhileAST, AsgnAST, DerefAST, \
											IndexOpAST, VoidAST, AddressAST, FloatLitAST, BreakAST, \
											ContinueAST, LoopAST, CharLitAST
from .analyzer                       import lookupSymbol
from .                               import types

class FundamentalType:
	def __init__(self, byteSize, isFloatType, isSigned=True):
		self.byteSize = byteSize
		self.isFloatType = isFloatType
		self.isSigned = isSigned
	
	def __str__(self):
		return '{}{}'.format('f' if self.isFloatType else 'i', self.byteSize * 8)
	
	@staticmethod
	def fromResolvedType(resolvedType):
		if resolvedType.isIntType or resolvedType.isPtrType or \
			(resolvedType.isOptionType and resolvedType.byteSize <= 8) or \
			resolvedType == types.Byte or resolvedType == types.Bool or resolvedType == types.Char:
			if resolvedType.isSigned:
				if resolvedType.byteSize == 1:
					return I8
				elif resolvedType.byteSize == 2:
					return I16
				elif resolvedType.byteSize == 4:
					return I32
				elif resolvedType.byteSize == 8:
					return I64
			else:
				if resolvedType.byteSize == 1:
					return U8
				elif resolvedType.byteSize == 2:
					return U16
				elif resolvedType.byteSize == 4:
					return U32
				elif resolvedType.byteSize == 8:
					return U64
		elif resolvedType.isFloatType:
			if resolvedType.byteSize == 4:
				return F32
			elif resolvedType.byteSize == 8:
				return F64
		
		assert 0

I8  = FundamentalType(1, False)
U8  = FundamentalType(1, False, False)
I16 = FundamentalType(2, False)
U16 = FundamentalType(2, False, False)
I32 = FundamentalType(4, False)
U32 = FundamentalType(4, False, False)
I64 = FundamentalType(8, False)
U64 = FundamentalType(8, False, False)
F32 = FundamentalType(4, True)
F64 = FundamentalType(8, True)

IPTR = U64 # platform dependent

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
			'&' if self.storage == Storage.STATIC or self.storage == Storage.LABEL else \
			''
		
		return '{} {}{}'.format(self.type, prefix, self.value)

class Instr:
	def __init__(self, ast, producesValue=False):
		self.ast = ast
		self.producesValue = producesValue
	
	def __str__(self):
		return 'Instr'
	
	def __repr__(self):
		return self.__str__()

class Move(Instr):
	def __init__(self, ast, src, dest, type, srcDeref=0, destDeref=0):
		super().__init__(ast, True)
		if srcDeref == 0:
			assert src.type.byteSize == dest.type.byteSize
			assert src.type.byteSize == type.byteSize
			assert src.type.isFloatType == dest.type.isFloatType
			assert src.type.isFloatType == type.isFloatType
		else:
			assert src.type == IPTR
		
		self.src = src
		self.dest = dest
		self.type = type
		self.srcDeref = srcDeref
		self.destDeref = destDeref
	
	def __str__(self):
		return '{}{} = {}'.format('^' * self.srcDeref, self.dest, self.src)

class Addr(Instr):
	def __init__(self, ast, src, dest):
		super().__init__(ast, True)
		assert dest.type == IPTR
		self.src = src
		self.dest = dest
	
	def __str__(self):
		return '{} = {}&'.format(self.dest, self.src)

class Call(Instr):
	def __init__(self, ast, callee, args, dest, cVarArgs):
		super().__init__(ast, True)
		self.callee = callee
		self.args = args
		self.dest = dest
		self.cVarArgs = cVarArgs
	
	def __str__(self):
		return '{} = call {}({})'.format(
			self.dest, self.callee, ', '.join([str(arg) for arg in self.args]))

class Ret(Instr):
	def __init__(self, ast, target):
		super().__init__(ast)
		self.target = target
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return 'ret {}'.format(target)

class Break(Instr):
	def __init__(self, ast):
		super().__init__(ast)
	
	def __str__(self):
		return 'break'

class Cont(Instr):
	def __init__(self, ast):
		super().__init__(ast)
	
	def __str__(self):
		return 'cont'

class IfElse(Instr):
	def __init__(self, ast, target, dest, ifBlock, elseBlock):
		super().__init__(ast, dest != None)
		self.target = target
		self.dest = dest
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return 'if {}\n{}\telse\n{}'.format(self.target, blockToStr(self.ifBlock, 2), blockToStr(self.elseBlock, 2))

class While(Instr):
	def __init__(self, ast, target, condBlock, block):
		super().__init__(ast)
		self.target = target
		self.condBlock = condBlock
		self.block = block
	
	def __str__(self):
		target = '' if self.target == None else ' {}'.format(self.target)
		return '{}\n\twhile {}\n{}{}'.format(blockToStr(self.condBlock, 2), self.target, blockToStr(self.block, 2), blockToStr(self.condBlock, 2))

class Loop(Instr):
	def __init__(self, ast, block):
		super().__init__(ast)
		self.block = block
	
	def __str__(self):
		return 'loop\n{}'.format(blockToStr(self.block, 2))

class BinOp(Instr):
	def __init__(self, ast, l, r, dest, type):
		super().__init__(ast, True)
		assert l.type.byteSize == r.type.byteSize
		assert l.type.byteSize == type.byteSize
		assert l.type.isFloatType == r.type.isFloatType
		assert l.type.isFloatType == dest.type.isFloatType
		assert l.type.isFloatType == type.isFloatType
		
		self.l = l
		self.r = r
		self.dest = dest
		self.type = type

class Cmp(BinOp):
	def __init__(self, ast, l, r, op, dest, type):
		super().__init__(ast, l, r, dest, type)
		self.op = op
	
	def __str__(self):
		return '{} = {} {} {}'.format(self.dest, self.l, self.op.desc(), self.r)

class Add(BinOp):
	def __init__(self, ast, l, r, dest, type):
		super().__init__(ast, l, r, dest, type)
		assert l.type.byteSize == dest.type.byteSize
	
	def __str__(self):
		return '{} = {} + {}'.format(self.dest, self.l, self.r)

class Sub(BinOp):
	def __init__(self, ast, l, r, dest, type):
		super().__init__(ast, l, r, dest, type)
		assert l.type.byteSize == dest.type.byteSize
	
	def __str__(self):
		return '{} = {} - {}'.format(self.dest, self.l, self.r)

class Mul(BinOp):
	def __init__(self, ast, l, r, dest, type):
		super().__init__(ast, l, r, dest, type)
		assert l.type.byteSize == dest.type.byteSize
	
	def __str__(self):
		return '{} = {} * {}'.format(self.dest, self.l, self.r)

class Div(BinOp):
	def __init__(self, ast, l, r, dest, type):
		super().__init__(ast, l, r, dest, type)
		assert l.type.byteSize == dest.type.byteSize
	
	def __str__(self):
		return '{} = {} / {}'.format(self.dest, self.l, self.r)

class Coerce(Instr):
	def __init__(self, ast, src, dest):
		super().__init__(ast, True)
		self.src = src
		self.dest = dest
	
	def __str__(self):
		return '{} = {} as {}'.format(self.dest, self.src, self.dest.type)

def returnToIR(scope, ret, fn, block):
	target = None
	if ret.expr != None:
		exprToIR(scope, ret.expr, fn, block)
		target = block[-1].dest.clone()
	
	block.append(Ret(ret, target))

def letToIR(scope, let, fn, block):
	if let.expr == None:
		src = Target(Storage.IMM, FundamentalType.fromResolvedType(let.resolvedSymbolType), 0)
	else:
		exprToIR(scope, let.expr, fn, block)
		if let.expr.resolvedType == types.Void:
			return
		src = block[-1].dest.clone()
	
	if let.noBinding:
		return
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, src.type, fn.sp)
	fn.locals[let.name] = fn.sp
	
	block.append(Move(let, src, dest, src.type))

def asgnToIR(scope, asgn, fn, block):
	exprToIR(scope, asgn.rvalue, fn, block)
	
	if asgn.rvalue.resolvedType == types.Void:
		return
	
	src = block[-1].dest.clone()
	
	targetSymbol = asgn.lvalue
	destDeref = 0
	if type(targetSymbol) == DerefAST:
		destDeref = targetSymbol.derefCount
		targetSymbol = targetSymbol.expr
	
	dest = Target(Storage.LOCAL, FundamentalType.fromResolvedType(asgn.rvalue.resolvedType), fn.locals[targetSymbol.name])
	
	block.append(Move(asgn, src, dest, src.type, 0, destDeref))

def indexToIR(scope, ind, fn, block):
	exprToIR(scope, ind.expr, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, ind.index, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, IPTR, fn.sp)
	
	block.append(Add(ind, l, r, dest, IPTR))
	
	fn.sp += 1
	
	src = block[-1].dest
	dest = Target(Storage.LOCAL, FundamentalType.fromResolvedType(ind.resolvedType), fn.sp)
	
	block.append(Move(ind, src, dest, dest.type, 1))

def ifBlockToIR(scope, ifAst, fn, block):
	exprToIR(scope, ifAst.expr, fn, block)
	result = block[-1].dest.clone()
	
	ifBlock = []
	blockToIR(scope, ifAst.ifBlock.exprs, fn, ifBlock)
	
	elseBlock = []
	if ifAst.elseBlock != None:
		blockToIR(scope, ifAst.elseBlock.exprs, fn, elseBlock)
	
	if ifAst.resolvedType == types.Void:
		dest = None
	else:
		fn.sp += 1
		dest = Target(Storage.LOCAL, FundamentalType.fromResolvedType(ifAst.resolvedType), fn.sp)
	
	block.append(IfElse(ifAst, result, dest, ifBlock, elseBlock))

def whileBlockToIR(scope, whileAst, fn, block):
	condBlock = []
	exprToIR(scope, whileAst.expr, fn, condBlock)
	result = condBlock[-1].dest.clone()
	
	whileBlock = []
	blockToIR(scope, whileAst.block.exprs, fn, whileBlock)
	block.append(While(whileAst, result, condBlock, whileBlock))

def loopToIR(scope, loop, fn, block):
	loopBlock = []
	blockToIR(scope, loop.block.exprs, fn, loopBlock)
	block.append(Loop(loop, loopBlock))

def boolLitToIR(scope, lit, fn, block):
	fn.sp += 1
	
	src = Target(Storage.IMM, I8, 1 if lit.value else 0)
	dest = Target(Storage.LOCAL, I8, fn.sp)
	
	block.append(Move(lit, src, dest, I8))

def charLitToIR(scope, lit, fn, block):
	fn.sp += 1
	
	src = Target(Storage.IMM, I32, lit.value)
	dest = Target(Storage.LOCAL, I32, fn.sp)
	
	block.append(Move(lit, src, dest, I32))

def intLitToIR(scope, lit, fn, block):
	fn.sp += 1
	
	type = FundamentalType.fromResolvedType(lit.resolvedType)
	src = Target(Storage.IMM, type, lit.value)
	dest = Target(Storage.LOCAL, type, fn.sp)
	
	block.append(Move(lit, src, dest, type))

def floatLitToIR(scope, lit, fn, block):
	fn.sp += 1
	
	type = FundamentalType.fromResolvedType(lit.resolvedType)
	src = Target(Storage.IMM, type, lit.value)
	dest = Target(Storage.LOCAL, type, fn.sp)
	
	block.append(Move(lit, src, dest, type))

def strLitToIR(scope, lit, fn, block):
	addr = len(fn.staticDefs)
	fn.staticDefs.append(lit.value)
	fn.sp += 1
	
	src = Target(Storage.STATIC, IPTR, addr)
	dest = Target(Storage.LOCAL, IPTR, fn.sp)
	
	block.append(Move(lit, src, dest, IPTR))

def fnCallToIR(scope, fnCall, fn, block):
	args = []
	for expr in fnCall.args:
		exprToIR(scope, expr, fn, block)
		args.append(block[-1].dest.clone())
	
	if type(fnCall.expr) == ValueRefAST:
		src = Target(Storage.LABEL, IPTR, lookupSymbol(None, scope, fnCall.expr, False).mangledName)
	else:
		exprToIR(scope, fnCall.expr, fn, block)
		src = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, IPTR, fn.sp)
	
	block.append(Call(fnCall, src, args, dest, fnCall.expr.resolvedType.cVarArgs))

def derefToIR(scope, expr, fn, block):
	exprToIR(scope, expr.expr, fn, block)
	
	fn.sp += 1
	
	src = block[-1].dest
	dest = Target(Storage.LOCAL, FundamentalType.fromResolvedType(expr.resolvedType), fn.sp)
	
	block.append(Move(expr, src, dest, dest.type, expr.derefCount))

def valueRefToIR(scope, expr, fn, block):
	fn.sp += 1
	
	type = FundamentalType.fromResolvedType(expr.resolvedType)
	src = Target(Storage.LABEL, type, expr.name)
	dest = Target(Storage.LOCAL, type, fn.sp)
	
	block.append(Move(expr, src, dest, type))

def listToIR(scope, exprs, fn, block):
	for expr in exprs:
		exprToIR(scope, expr, fn, block)

def addToIR(scope, op, fn, block):
	exprToIR(scope, op.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, op.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, l.type, fn.sp)
	
	block.append(Add(op, l, r, dest, l.type))

def subToIR(scope, op, fn, block):
	exprToIR(scope, op.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, op.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, l.type, fn.sp)
	
	block.append(Sub(op, l, r, dest, l.type))

def mulToIR(scope, op, fn, block):
	exprToIR(scope, op.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, op.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, l.type, fn.sp)
	
	block.append(Mul(op, l, r, dest, l.type))

def divToIR(scope, op, fn, block):
	exprToIR(scope, op.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, op.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, l.type, fn.sp)
	
	block.append(Div(op, l, r, dest, l.type))

def cmpToIR(scope, op, fn, block):
	exprToIR(scope, op.l, fn, block)
	l = block[-1].dest.clone()
	
	exprToIR(scope, op.r, fn, block)
	r = block[-1].dest.clone()
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, I64, fn.sp)
	
	block.append(Cmp(op, l, r, op.op, dest, l.type))

def coercionToIR(scope, coercion, fn, block):
	exprToIR(scope, coercion.expr, fn, block)
	
	if coercion.typeRef.resolvedType == types.Void:
		return
	
	fn.sp += 1
	src = block[-1].dest.clone()
	dest = Target(Storage.LOCAL, FundamentalType.fromResolvedType(coercion.typeRef.resolvedType), fn.sp)
	block.append(Coerce(coercion, src, dest))

def addressToIR(scope, addr, fn, block):
	if type(addr.expr) != ValueRefAST:
		assert 0
	
	src = Target(Storage.LABEL, FundamentalType.fromResolvedType(addr.expr.resolvedType), addr.expr.name)
	
	fn.sp += 1
	dest = Target(Storage.LOCAL, IPTR, fn.sp)
	
	block.append(Addr(addr, src, dest))

def breakToIR(scope, expr, fn, block):
	block.append(Break(expr))

def continueToIR(scope, expr, fn, block):
	block.append(Cont(expr))

def exprToIR(scope, expr, fn, block):
	if type(expr) == BoolLitAST:
		boolLitToIR(scope, expr, fn, block)
	elif type(expr) == IntLitAST:
		intLitToIR(scope, expr, fn, block)
	elif type(expr) == FloatLitAST:
		floatLitToIR(scope, expr, fn, block)
	elif type(expr) == StrLitAST:
		strLitToIR(scope, expr, fn, block)
	elif type(expr) == CharLitAST:
		charLitToIR(scope, expr, fn, block)
	elif type(expr) == ValueRefAST:
		valueRefToIR(scope, expr, fn, block)
	elif type(expr) == DerefAST:
		derefToIR(scope, expr, fn, block)
	elif type(expr) == AddressAST:
		addressToIR(scope, expr, fn, block)
	elif type(expr) == FnCallAST:
		fnCallToIR(scope, expr, fn, block)
	elif type(expr) == LetAST:
		letToIR(scope, expr, fn, block)
	elif type(expr) == IfAST:
		ifBlockToIR(scope, expr, fn, block)
	elif type(expr) == LoopAST:
		loopToIR(scope, expr, fn, block)
	elif type(expr) == WhileAST:
		whileBlockToIR(scope, expr, fn, block)
	elif type(expr) == ReturnAST:
		returnToIR(scope, expr, fn, block)
	elif type(expr) == BreakAST:
		breakToIR(scope, expr, fn, block)
	elif type(expr) == ContinueAST:
		continueToIR(scope, expr, fn, block)
	elif type(expr) == BlockAST:
		listToIR(scope, expr.exprs, fn, block)
	elif type(expr) == CoercionAST:
		coercionToIR(scope, expr, fn, block)
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