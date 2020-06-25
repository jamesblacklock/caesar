from enum    import Enum
from ..token import TokenType

class Contract:
	def __init__(self, symbol, enumType, variants, indLevel):
		self.symbol = symbol
		self.enumType = enumType
		self.variants = variants
		self.indLevel = indLevel
	
	def inverted(self):
		variants = [v for v in self.enumType.variants if v not in self.variants]
		return Contract(self.symbol, self.enumType, variants, self.indLevel)
	
	def intersect(self, other):
		variants = [v for v in self.variants if v in other.variants]
		return Contract(self.symbol, self.enumType, variants, self.indLevel)

class AST:
	def __init__(self, span):
		self.attrs = []
		self.attrsInvoked = False
		self.span = span
		self.hasValue = False
		self.analyzed = False
	
	def setAnalyzed(self):
		self.analyzed = True
	
	def analyze(self, state, implicitType):
		assert 0

class Symbol(AST):
	def __init__(self, nameTok, span, doccomment=None, extern=False):
		super().__init__(span)
		self.nameTok = nameTok
		self.name = nameTok.content if nameTok else None
		self.unused = True
		self.symbolTable = None
		self.doccomment = doccomment
		self.extern = extern
		self.pub = False

class ValueSymbol(Symbol):
	def __init__(self, nameTok, typeRef, span, doccomment=None, extern=False):
		super().__init__(nameTok, span, doccomment, extern)
		self.typeRef = typeRef
		# self.type = None
		# self.typeModifiers = TypeModifiers()
		# self.contracts = None

class ValueExpr(AST):
	def __init__(self, span):
		super().__init__(span)
		self.type = None
		self.typeModifiers = None
		self.hasValue = True
		self.contracts = None

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