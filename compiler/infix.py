from enum       import Enum
from .token     import TokenType
from .ast       import ValueExpr
from .primitive import IntLit
from .coercion  import Coercion
from .block     import Block
from .ifexpr    import If
from .types     import canPromote, typesMatch, canAccommodate, hasDefiniteType, \
                       Int32, Int64, UInt64, ISize, USize, Bool, Byte, Char
from .ir        import FAdd, FSub, FMul, FDiv, FEq, FNEq, FGreater, FLess, FGreaterEq, FLessEq, \
                       Add, Sub, Mul, Div, Mod, Eq, NEq, Greater, Less, GreaterEq, LessEq, Imm, IPTR
from .log       import logError

class InfixOps(Enum):
	ARROW = '->'
	LSHIFT = '<<'
	RSHIFT = '>>'
	TIMES = '*'
	DIV = '/'
	MODULO = '%'
	PLUS = '+'
	MINUS = '-'
	BITAND = '&'
	BITOR = '|'
	BITXOR = '^'
	RNGCLOSED = '..<'
	RNGOPEN = '...'
	EQ = '=='
	NEQ = '!='
	GREATER = '>'
	LESS = '<'
	GREATEREQ = '>='
	LESSEQ = '<='
	AND = '&&'
	OR = '||'
		
	@staticmethod
	def fromTokenType(type):
		if type == TokenType.ARROW:
			return InfixOps.ARROW
		elif type == TokenType.LSHIFT:
			return InfixOps.LSHIFT
		elif type == TokenType.RSHIFT:
			return InfixOps.RSHIFT
		elif type == TokenType.TIMES:
			return InfixOps.TIMES
		elif type == TokenType.DIV:
			return InfixOps.DIV
		elif type == TokenType.MODULO:
			return InfixOps.MODULO
		elif type == TokenType.PLUS:
			return InfixOps.PLUS
		elif type == TokenType.MINUS:
			return InfixOps.MINUS
		elif type == TokenType.AMP:
			return InfixOps.BITAND
		elif type == TokenType.PIPE:
			return InfixOps.BITOR
		elif type == TokenType.CARET:
			return InfixOps.BITXOR
		elif type == TokenType.ELLIPSIS:
			return InfixOps.RNGCLOSED
		elif type == TokenType.RNGOPEN:
			return InfixOps.RNGOPEN
		elif type == TokenType.EQ:
			return InfixOps.EQ
		elif type == TokenType.NEQ:
			return InfixOps.NEQ
		elif type == TokenType.GREATER:
			return InfixOps.GREATER
		elif type == TokenType.LESS:
			return InfixOps.LESS
		elif type == TokenType.GREATEREQ:
			return InfixOps.GREATEREQ
		elif type == TokenType.LESSEQ:
			return InfixOps.LESSEQ
		elif type == TokenType.AND:
			return InfixOps.AND
		elif type == TokenType.OR:
			return InfixOps.OR
		else:
			return None

ARITHMETIC_OPS = (
	InfixOps.TIMES,
	InfixOps.DIV,
	InfixOps.MODULO,
	InfixOps.PLUS,
	InfixOps.MINUS
)

BITWISE_OPS = (
	InfixOps.BITAND,
	InfixOps.BITOR,
	InfixOps.BITXOR
)

BITSHIFT_OPS = (
	InfixOps.LSHIFT,
	InfixOps.RSHIFT
)

CMP_OPS = (
	InfixOps.EQ,
	InfixOps.NEQ,
	InfixOps.GREATER,
	InfixOps.LESS,
	InfixOps.GREATEREQ,
	InfixOps.LESSEQ
)

LOGIC_OPS = (
	InfixOps.AND,
	InfixOps.OR
)

PTR_PTR_OPS = (
	InfixOps.MINUS,
	InfixOps.EQ,
	InfixOps.NEQ,
	InfixOps.GREATER,
	InfixOps.LESS,
	InfixOps.GREATEREQ,
	InfixOps.LESSEQ
)

PTR_INT_OPS = (
	InfixOps.PLUS,
	InfixOps.MINUS
)

INT_PTR_OPS = (
	InfixOps.PLUS,
)

RNG_OPS = (
	InfixOps.RNGCLOSED,
	InfixOps.RNGOPEN
)

class InfixOp(ValueExpr):
	def __init__(self, l, r, op, opTok, span):
		super().__init__(span)
		self.l = l
		self.r = r
		self.op = op
		self.opTok = opTok
	
	def analyze(infixOp, state, implicitType):
		def opErr(lType, rType):
			logError(state, infixOp.opTok.span, 'invalid operand types for operator `{}` ({} and {})'
				.format(infixOp.op.value, lType, rType))
		
		lIndefinite = not hasDefiniteType(infixOp.l)
		rIndefinite = not hasDefiniteType(infixOp.r)
		if lIndefinite and rIndefinite:
			if type(infixOp.l) == IntLit and type(infixOp.r) == IntLit:
				if canAccommodate(Int32, infixOp.l.value) and \
					canAccommodate(Int32, infixOp.r.value):
					infixOp.r = state.analyzeNode(infixOp.r, Int32)
					infixOp.l = state.analyzeNode(infixOp.l, Int32)
				elif canAccommodate(Int64, infixOp.l.value) and \
					canAccommodate(Int64, infixOp.r.value):
					infixOp.r = state.analyzeNode(infixOp.r, Int64)
					infixOp.l = state.analyzeNode(infixOp.l, Int64)
				else:
					infixOp.r = state.analyzeNode(infixOp.r, UInt64)
					infixOp.l = state.analyzeNode(infixOp.l, UInt64)
			else:
				assert 0
		elif rIndefinite and type(infixOp.l) not in (Block, If):
			infixOp.l = state.analyzeNode(infixOp.l, implicitType)
			if type(infixOp.r) == IntLit and infixOp.l.type and infixOp.l.type.isPtrType:
				# if infixOp.op in PTR_INT_OPS:
					infixOp.r = state.analyzeNode(infixOp.r, ISize if infixOp.r.value < 0 else USize)
				# else:
				# 	opErr()
				# 	return
			else:
				infixOp.r = state.analyzeNode(infixOp.r, infixOp.l.type)
		elif lIndefinite or type(infixOp.l) in (Block, If):
			infixOp.r = state.analyzeNode(infixOp.r, implicitType)
			if type(infixOp.l) == IntLit and infixOp.r.type and infixOp.r.type.isPtrType:
				# if infixOp.op in PTR_INT_OPS:
					infixOp.l = state.analyzeNode(infixOp.l, ISize if infixOp.l.value < 0 else USize)
				# else:
					# opErr()
					# return
			else:
				infixOp.l = state.analyzeNode(infixOp.l, infixOp.r.type)
		else:
			infixOp.l = state.analyzeNode(infixOp.l, implicitType)
			infixOp.r = state.analyzeNode(infixOp.r, implicitType)
		
		lType = infixOp.l.type
		# if lType.isOwnedType:# or rType.isRenamedType:
			# lType = lType.baseType
		
		rType = infixOp.r.type
		# if rType.isOwnedType:# or rType.isRenamedType:
			# rType = rType.baseType
		
		if not lType or not rType:
			return
		
		if canPromote(lType, rType):
			infixOp.l = Coercion(infixOp.l, None, infixOp.l.span, resolvedType=rType)
			lType = rType
		elif canPromote(rType, lType):
			infixOp.r = Coercion(infixOp.r, None, infixOp.r.span, resolvedType=lType)
			rType = lType
		
		if lType == Bool and rType == Bool:
			if infixOp.op == InfixOps.EQ:
				infixOp.type = Bool
				return
		elif lType == Byte and rType == Byte:
			if infixOp.op == InfixOps.EQ:
				infixOp.type = Bool
				return
			if infixOp.op in BITWISE_OPS:
				infixOp.type = Byte
				return
		elif lType == Char and rType == Char:
			if infixOp.op == InfixOps.EQ:
				infixOp.type = Bool
				return
		elif lType.isPtrType and rType.isIntType:
			if infixOp.op in PTR_INT_OPS and rType.byteSize == USize.byteSize:
				infixOp.type = lType
				return
		elif lType.isIntType and rType.isPtrType:
			if infixOp.op in INT_PTR_OPS and infixOp.l.type.byteSize == USize.byteSize:
				infixOp.type = rType
				return
		elif lType.isPtrType and rType.isPtrType:
			if infixOp.op in PTR_PTR_OPS and typesMatch(lType, rType):
				infixOp.type = lType
				return
		elif typesMatch(lType, rType) and (lType.isIntType or lType.isFloatType):
			if infixOp.op == InfixOps.MODULO:
				if lType.isIntType:
					infixOp.type = lType
					return
			elif infixOp.op in ARITHMETIC_OPS:
				infixOp.type = lType
				return
			elif (infixOp.op in BITWISE_OPS or infixOp.op in BITSHIFT_OPS) and lType.isIntType:
				infixOp.type = lType
				return
			elif infixOp.op in CMP_OPS:
				infixOp.type = Bool
				return
		
		opErr(lType, rType)
	
	def accessSymbols(self, scope):
		self.l.accessSymbols(scope)
		self.r.accessSymbols(scope)
	
	def writeIR(ast, state):
		if ast.op == InfixOps.PLUS:
			ast.writeAddIR(state)
		elif ast.op == InfixOps.MINUS:
			ast.writeSubIR(state)
		elif ast.op == InfixOps.TIMES:
			ast.writeMulIR(state)
		elif ast.op == InfixOps.DIV:
			ast.writeDivIR(state)
		elif ast.op == InfixOps.MODULO:
			ast.writeModuloIR(state)
		elif ast.op in CMP_OPS:
			ast.writeCmpIR(state)
		else:
			assert 0
	
	def writeAddIR(ast, state):
		ast.l.writeIR(state)
		
		if ast.l.type.isPtrType:
			ast.r.writeIR(state)
			if ast.l.type.indLevel == 1:
				mul = ast.l.type.baseType.byteSize
			else:
				mul = IPTR.byteSize
			state.appendInstr(Imm(ast, IPTR, mul))
			state.appendInstr(Mul(ast))
			state.appendInstr(Add(ast))
		elif ast.r.type.isPtrType:
			if ast.r.type.indLevel == 1:
				mul = ast.r.type.baseType.byteSize
			else:
				mul = IPTR.byteSize
			state.appendInstr(Imm(ast, IPTR, mul))
			state.appendInstr(Mul(ast))
			ast.r.writeIR(state)
			state.appendInstr(Add(ast))
		elif ast.l.type.isFloatType:
			ast.r.writeIR(state)
			state.appendInstr(FAdd(ast))
		else:
			ast.r.writeIR(state)
			state.appendInstr(Add(ast))

	def writeSubIR(ast, state):
		ast.l.writeIR(state)
		
		if ast.l.type.isPtrType:
			ast.r.writeIR(state)
			if ast.l.type.indLevel == 1:
				mul = ast.l.type.baseType.byteSize
			else:
				mul = IPTR.byteSize
			state.appendInstr(Imm(ast, IPTR, mul))
			state.appendInstr(Mul(ast))
			state.appendInstr(Sub(ast))
		elif ast.l.type.isFloatType:
			ast.r.writeIR(state)
			state.appendInstr(FSub(ast))
		else:
			ast.r.writeIR(state)
			state.appendInstr(Sub(ast))
	
	def writeMulIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		if ast.l.type.isFloatType:
			state.appendInstr(FMul(ast))
		else:
			state.appendInstr(Mul(ast))
	
	def writeDivIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		if ast.l.type.isFloatType:
			state.appendInstr(FDiv(ast))
		else:
			state.appendInstr(Div(ast))
	
	def writeModuloIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		state.appendInstr(Mod(ast))
	
	def writeCmpIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		
		if ast.op == InfixOps.EQ:
			if ast.l.type.isFloatType:
				instr = FEq(ast)
			else:
				instr = Eq(ast)
		elif ast.op == InfixOps.NEQ:
			if ast.l.type.isFloatType:
				instr = FNEq(ast)
			else:
				instr = NEq(ast)
		elif ast.op == InfixOps.GREATER:
			if ast.l.type.isFloatType:
				instr = FGreater(ast)
			else:
				instr = Greater(ast)
		elif ast.op == InfixOps.LESS:
			if ast.l.type.isFloatType:
				instr = FLess(ast)
			else:
				instr = Less(ast)
		elif ast.op == InfixOps.GREATEREQ:
			if ast.l.type.isFloatType:
				instr = FGreaterEq(ast)
			else:
				instr = GreaterEq(ast)
		elif ast.op == InfixOps.LESSEQ:
			if ast.l.type.isFloatType:
				instr = FLessEq(ast)
			else:
				instr = LessEq(ast)
		else:
			assert 0
		
		state.appendInstr(instr)
	
	def pretty(self, output, indent=0):
		self.l.pretty(output, indent)
		output.write(' {} '.format(self.op.value))
		self.r.pretty(output, indent)
