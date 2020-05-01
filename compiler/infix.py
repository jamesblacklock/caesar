from enum       import Enum
from .token     import TokenType
from .ast       import ValueExpr
from .primitive import IntLit
from .types     import hasDefiniteType, Int32, Int64, UInt64, ISize, USize, Bool, Byte, Char
from .ir        import Imm, Add, Sub, Mul, Div, Eq, NEq, Greater, Less, GreaterEq, LessEq, IPTR
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
		opErr = lambda: \
			logError(state, infixOp.opTok.span, 
				'invalid operand types for operator `{}` ({} and {})'
					.format(infixOp.op.value, infixOp.l.type, infixOp.r.type))
		
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
		elif rIndefinite:
			infixOp.l = state.analyzeNode(infixOp.l, implicitType)
			if type(infixOp.r) == IntLit:
				if infixOp.l.type and infixOp.l.type.isPtrType:
					if infixOp.op in PTR_INT_OPS:
						infixOp.r = state.analyzeNode(infixOp.r, ISize if infixOp.r.value < 0 else USize)
					else:
						opErr()
						return
				else:
					infixOp.r = state.analyzeNode(infixOp.r, infixOp.l.type)
			else:
				assert 0
		elif lIndefinite:
			infixOp.r = state.analyzeNode(infixOp.r, implicitType)
			if type(infixOp.l) == IntLit:
				if infixOp.r.type and infixOp.r.type.isPtrType:
					if infixOp.op in PTR_INT_OPS:
						infixOp.l = state.analyzeNode(infixOp.l, ISize if infixOp.l.value < 0 else USize)
					else:
						opErr()
						return
				else:
					infixOp.l = state.analyzeNode(infixOp.l, infixOp.r.type)
			else:
				assert 0
		else:
			infixOp.l = state.analyzeNode(infixOp.l, implicitType)
			infixOp.r = state.analyzeNode(infixOp.r, implicitType)
		
		if not infixOp.l.type or not infixOp.r.type:
			return
		
		if infixOp.l.type == Bool and infixOp.r.type == Bool:
			if infixOp.op == InfixOp.EQ:
				infixOp.type = Bool
				return
		elif infixOp.l.type == Byte and infixOp.r.type == Byte:
			if infixOp.op == InfixOp.EQ:
				infixOp.type = Bool
				return
		elif infixOp.l.type == Char and infixOp.r.type == Char:
			if infixOp.op == InfixOp.EQ:
				infixOp.type = Bool
				return
		elif infixOp.l.type.isPtrType and infixOp.r.type.isIntType:
			if infixOp.op in PTR_INT_OPS and infixOp.r.type.byteSize == USize.byteSize:
				infixOp.type = infixOp.l.type
				return
		elif infixOp.l.type.isIntType and infixOp.r.type.isPtrType:
			if infixOp.op in INT_PTR_OPS and infixOp.l.type.byteSize == USize.byteSize:
				infixOp.type = infixOp.r.type
				return
		elif infixOp.l.type.isPtrType and infixOp.r.type.isPtrType:
			if infixOp.op in PTR_PTR_OPS and getValidAssignType(infixOp.l.type, infixOp.r.type):
				infixOp.type = infixOp.l.type
				return
		elif infixOp.l.type.isIntType and infixOp.r.type.isIntType:
			lType = infixOp.l.type
			rType = infixOp.r.type
			
			if lType.byteSize == rType.byteSize and lType.isSigned == rType.isSigned:
				if infixOp.op in ARITHMETIC_OPS or infixOp.op in BITWISE_OPS:
					infixOp.type = lType
					return
				elif infixOp.op in CMP_OPS:
					infixOp.type = Bool
					return
			
			if infixOp.op in BITSHIFT_OPS:
				infixOp.type = lType
				return
		
		opErr()
	
	def writeIR(ast, state):
		if ast.op == InfixOps.PLUS:
			ast.writeAddIR(state)
		elif ast.op == InfixOps.MINUS:
			ast.writeSubIR(state)
		elif ast.op == InfixOps.TIMES:
			ast.writeMulIR(state)
		elif ast.op == InfixOps.DIV:
			ast.writeDivIR(state)
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
		else:
			ast.r.writeIR(state)
			state.appendInstr(Sub(ast))

	def writeMulIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		state.appendInstr(Mul(ast))

	def writeDivIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		state.appendInstr(Div(ast))

	def writeCmpIR(ast, state):
		ast.l.writeIR(state)
		ast.r.writeIR(state)
		
		if ast.op == InfixOps.EQ:
			instr = Eq(ast)
		elif ast.op == InfixOps.NEQ:
			instr = NEq(ast)
		elif ast.op == InfixOps.GREATER:
			instr = Greater(ast)
		elif ast.op == InfixOps.LESS:
			instr = Less(ast)
		elif ast.op == InfixOps.GREATEREQ:
			instr = GreaterEq(ast)
		elif ast.op == InfixOps.LESSEQ:
			instr = LessEq(ast)
		else:
			assert 0
		
		state.appendInstr(instr)
	
	def pretty(self, output, indent=0):
		self.l.pretty(output, indent)
		output.write(' {} '.format(self.op.value))
		self.r.pretty(output, indent)