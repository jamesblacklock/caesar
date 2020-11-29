from enum    import Enum

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
	AND = 'and'
	OR = 'or'

CMP_OPS = (
	InfixOps.EQ,
	InfixOps.NEQ,
	InfixOps.GREATER,
	InfixOps.LESS,
	InfixOps.GREATEREQ,
	InfixOps.LESSEQ
)

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
