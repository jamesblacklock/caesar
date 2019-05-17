from enum import Enum
from .token import TokenType

# ->
# << >>
# * / %
# + -
# & ^ |
# ..< ...
# < <= > >= == !=
# &&
# ||

INFIX_PRECEDENCE = {
	TokenType.ARROW:     900, 
	TokenType.LSHIFT:    800, 
	TokenType.RSHIFT:    800, 
	TokenType.TIMES:     700,
	TokenType.DIV:       700,
	TokenType.MODULO:    700, 
	TokenType.PLUS:      600, 
	TokenType.MINUS:     600,
	TokenType.BITAND:    500, 
	TokenType.PIPE:      500, 
	TokenType.CARET:     500, 
	TokenType.RNGCLOSED: 400, 
	TokenType.RNGOPEN:   400, 
	TokenType.EQ:        300, 
	TokenType.NEQ:       300, 
	TokenType.GREATER:   300, 
	TokenType.LESS:      300,
	TokenType.GREATEREQ: 300, 
	TokenType.LESSEQ:    300,
	TokenType.AND:       200, 
	TokenType.OR:        100
}

class InfixOp(Enum):
	ARROW = 'ARROW'
	LSHIFT = 'LSHIFT'
	RSHIFT = 'RSHIFT'
	TIMES = 'TIMES'
	DIV = 'DIV'
	MODULO = 'MODULO'
	PLUS = 'PLUS'
	MINUS = 'MINUS'
	BITAND = 'BITAND'
	BITOR = 'BITOR'
	BITXOR = 'BITXOR'
	RNGCLOSED = 'RNGCLOSED'
	RNGOPEN = 'RNGOPEN'
	EQ = 'EQ'
	NEQ = 'NEQ'
	GREATER = 'GREATER'
	LESS = 'LESS'
	GREATEREQ = 'GREATEREQ'
	LESSEQ = 'LESSEQ'
	AND = 'AND'
	OR = 'OR'
	
	def desc(self):
		if self == InfixOp.ARROW:
			return '->'
		elif self == InfixOp.LSHIFT:
			return '<<'
		elif self == InfixOp.RSHIFT:
			return '>>'
		elif self == InfixOp.TIMES:
			return '*'
		elif self == InfixOp.DIV:
			return '/'
		elif self == InfixOp.MODULO:
			return '%'
		elif self == InfixOp.PLUS:
			return '+'
		elif self == InfixOp.MINUS:
			return '-'
		elif self == InfixOp.BITAND:
			return '&'
		elif self == InfixOp.BITOR:
			return '|'
		elif self == InfixOp.BITXOR:
			return '^'
		elif self == InfixOp.RNGCLOSED:
			return '..='
		elif self == InfixOp.RNGOPEN:
			return '..='
		elif self == InfixOp.EQ:
			return '=='
		elif self == InfixOp.NEQ:
			return '!='
		elif self == InfixOp.GREATER:
			return '>'
		elif self == InfixOp.LESS:
			return '<'
		elif self == InfixOp.GREATEREQ:
			return '>='
		elif self == InfixOp.LESSEQ:
			return '<='
		elif self == InfixOp.AND:
			return '&&'
		elif self == InfixOp.OR:
			return '||'		
		else:
			assert 0
		
	@staticmethod
	def fromTokenType(type):
		if type == TokenType.ARROW:
			return InfixOp.ARROW
		elif type == TokenType.LSHIFT:
			return InfixOp.LSHIFT
		elif type == TokenType.RSHIFT:
			return InfixOp.RSHIFT
		elif type == TokenType.TIMES:
			return InfixOp.TIMES
		elif type == TokenType.DIV:
			return InfixOp.DIV
		elif type == TokenType.MODULO:
			return InfixOp.MODULO
		elif type == TokenType.PLUS:
			return InfixOp.PLUS
		elif type == TokenType.MINUS:
			return InfixOp.MINUS
		elif type == TokenType.BITAND:
			return InfixOp.BITAND
		elif type == TokenType.PIPE:
			return InfixOp.BITOR
		elif type == TokenType.CARET:
			return InfixOp.BITXOR
		elif type == TokenType.RNGCLOSED:
			return InfixOp.RNGCLOSED
		elif type == TokenType.RNGOPEN:
			return InfixOp.RNGOPEN
		elif type == TokenType.EQ:
			return InfixOp.EQ
		elif type == TokenType.NEQ:
			return InfixOp.NEQ
		elif type == TokenType.GREATER:
			return InfixOp.GREATER
		elif type == TokenType.LESS:
			return InfixOp.LESS
		elif type == TokenType.GREATEREQ:
			return InfixOp.GREATEREQ
		elif type == TokenType.LESSEQ:
			return InfixOp.LESSEQ
		elif type == TokenType.AND:
			return InfixOp.AND
		elif type == TokenType.OR:
			return InfixOp.OR
		else:
			return None

ARITHMETIC_OPS = (
	InfixOp.TIMES,
	InfixOp.DIV,
	InfixOp.MODULO,
	InfixOp.PLUS,
	InfixOp.MINUS
)

BITWISE_OPS = (
	InfixOp.BITAND,
	InfixOp.BITOR,
	InfixOp.BITXOR
)

BITSHIFT_OPS = (
	InfixOp.LSHIFT,
	InfixOp.RSHIFT
)

CMP_OPS = (
	InfixOp.EQ,
	InfixOp.NEQ,
	InfixOp.GREATER,
	InfixOp.LESS,
	InfixOp.GREATEREQ,
	InfixOp.LESSEQ
)

LOGIC_OPS = (
	InfixOp.AND,
	InfixOp.OR
)

PTR_PTR_OPS = (
	InfixOp.MINUS,
	InfixOp.EQ,
	InfixOp.NEQ,
	InfixOp.GREATER,
	InfixOp.LESS,
	InfixOp.GREATEREQ,
	InfixOp.LESSEQ
)

PTR_INT_OPS = (
	InfixOp.PLUS,
	InfixOp.MINUS
)

INT_PTR_OPS = (
	InfixOp.PLUS
)

RNG_OPS = (
	InfixOp.RNGCLOSED,
	InfixOp.RNGOPEN
)

class ResolvedType:
	def __init__(self, name, byteSize, 
		isFnType=False, isMultiIntType=False, isPtrType=False, 
		isIntType=False, isOptionType=False, isVoidType=False,
		isPrimitiveType=False):
		self.name = name
		self.byteSize = byteSize
		self.isPrimitiveType = isPrimitiveType
		self.isVoidType = isVoidType
		self.isFnType = isFnType
		self.isMultiIntType = isMultiIntType
		self.isPtrType = isPtrType
		self.isIntType = isIntType
		self.isOptionType = isOptionType
	
	def __str__(self):
		return self.name

class ResolvedOptionType(ResolvedType):
	def __init__(self, *resolvedTypes):
		name = '|'.join([t.name for t in resolvedTypes])
		super().__init__(name, 0, isOptionType=True)
		self.resolvedTypes = resolvedTypes

class ResolvedFnType(ResolvedType):
	def __init__(self, resolvedParamTypes, cVarArgs, resolvedReturnType):
		name = 'fn ({}) -> {}'.format(
			', '.join([t.name for t in resolvedParamTypes]), 
			resolvedReturnType.name)
		super().__init__(name, 0, isFnType=True)
		self.resolvedReturnType = resolvedReturnType
		self.resolvedParamTypes = resolvedParamTypes
		self.cVarArgs = cVarArgs

PLATFORM_WORD_SIZE = 8

class ResolvedPtrType(ResolvedType):
	def __init__(self, baseType, indirectionLevel):
		name = baseType.name + ('^' * indirectionLevel)
		super().__init__(name, PLATFORM_WORD_SIZE, isPtrType=True, isPrimitiveType=True)
		self.baseType = baseType
		self.indirectionLevel = indirectionLevel

Void    = ResolvedType('Void',    0, isVoidType=True)
Bool    = ResolvedType('Bool',    1, isPrimitiveType=True)
Byte    = ResolvedType('Byte',    1, isPrimitiveType=True)
Int8    = ResolvedType('Int8',    1, isPrimitiveType=True, isIntType=True)
UInt8   = ResolvedType('UInt8',   1, isPrimitiveType=True, isIntType=True)
Int16   = ResolvedType('Int16',   2, isPrimitiveType=True, isIntType=True)
UInt16  = ResolvedType('UInt16',  2, isPrimitiveType=True, isIntType=True)
Int32   = ResolvedType('Int32',   4, isPrimitiveType=True, isIntType=True)
UInt32  = ResolvedType('UInt32',  4, isPrimitiveType=True, isIntType=True)
Char    = ResolvedType('Char',    4, isPrimitiveType=True)
Int64   = ResolvedType('Int64',   8, isPrimitiveType=True, isIntType=True)
UInt64  = ResolvedType('UInt64',  8, isPrimitiveType=True, isIntType=True)
ISize   = ResolvedType('ISize',   PLATFORM_WORD_SIZE, isPrimitiveType=True, isIntType=True)
USize   = ResolvedType('USize',   PLATFORM_WORD_SIZE, isPrimitiveType=True, isIntType=True)
Float32 = ResolvedType('Float32', 4, isPrimitiveType=True)
Float64 = ResolvedType('Float64', 8, isPrimitiveType=True)

BUILTIN_TYPES = [
	Void,
	Bool,
	Byte,
	Int8,
	UInt8,
	Int16,
	UInt16,
	Int32,
	UInt32,
	Char,
	Int64,
	UInt64,
	ISize,
	USize,
	Float32,
	Float64
]

I8_MAX  = 127
I16_MAX = 32767
I32_MAX = 2147483647
I64_MAX = 9223372036854775807
ISZ_MAX = I64_MAX
I8_MIN  = -I8_MAX  - 1
I16_MIN = -I16_MAX - 1
I32_MIN = -I32_MAX - 1
I64_MIN = -I64_MAX - 1
ISZ_MIN = -ISZ_MAX - 1
U8_MAX  = 255
U16_MAX = 65535
U32_MAX = 4294967295
U64_MAX = 18446744073709551615
USZ_MAX = U64_MAX

class ResolvedMultiIntType(ResolvedType):
	def __init__(self, possibleTypes):
		super().__init__('integer', 0, isMultiIntType=True, isIntType=True, isPrimitiveType=True)
		self.possibleTypes = possibleTypes
	
	def __str__(self):
		return self.possibleTypes[0].__str__() if len(self.possibleTypes) > 0 else self.name
	
	@staticmethod
	def fromValue(value):
		possibleTypes = []
		if value <= I8_MAX and value >= I8_MIN:
			possibleTypes.append(Int8)
		if value <= U8_MAX and value >= 0:
			possibleTypes.append(UInt8)
		if value <= I16_MAX and value >= I16_MIN:
			possibleTypes.append(Int16)
		if value <= U16_MAX and value >= 0:
			possibleTypes.append(UInt16)
		if value <= I32_MAX and value >= I32_MIN:
			possibleTypes.append(Int32)
		if value <= U32_MAX and value >= 0:
			possibleTypes.append(UInt32)
		if value <= I64_MAX and value >= I64_MIN:
			possibleTypes.append(Int64)
		if value <= U64_MAX and value >= 0:
			possibleTypes.append(UInt64)
		if value <= ISZ_MAX and value >= ISZ_MIN:
			possibleTypes.append(ISize)
		if value <= USZ_MAX and value >= 0:
			possibleTypes.append(USize)
		
		return ResolvedMultiIntType(possibleTypes)
	
	@staticmethod
	def inCommon(t1, t2):
		if t2.isMultiIntType:
			t1, t2 = t2, t1
		
		if t1.isMultiIntType:
			if t2.isMultiIntType:
				return [t for t in t1.possibleTypes if t in t2.possibleTypes]
			else:
				return [t2] if t2 in t1.possibleTypes else []
		else:
			return [t1] if t1 == t2 else []

def typesMatch(l, r):
	if l == r:
		return True
	
	if l.isPtrType and r.isPtrType and typesMatch(l.baseType, r.baseType):
		return True
	
	if l.isIntType and r.isIntType and len(ResolvedMultiIntType.inCommon(l, r)) > 0:
		return True
	
	if l.isOptionType and r.isOptionType and len(l.resolvedTypes) == len(r.resolvedTypes):
		for t in l.resolvedTypes:
			if t not in r.resolvedTypes:
				return False
		return True
	
	return False

def canAssignFrom(expectedType, foundType):
	if expectedType == None or foundType == None:
		return True
	
	if expectedType.isMultiIntType:
		expectedType, foundType = foundType, expectedType
	
	if foundType.isMultiIntType:
		if expectedType.isMultiIntType:
			for t in foundType.possibleTypes:
				if t in expectedType.possibleTypes:
					return True
			
			return False
		else:
			return expectedType in foundType.possibleTypes
	elif type(expectedType) == ResolvedPtrType and type(foundType) == ResolvedPtrType:
		return expectedType.baseType == foundType.baseType and \
			expectedType.indirectionLevel == foundType.indirectionLevel
	else:
		return expectedType is foundType

def canCoerce(fromType, toType):
	return fromType.isPrimitiveType and toType.isPrimitiveType