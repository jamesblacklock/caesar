import re
from enum                import Enum
from .token              import TokenType
from .err                import logError

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
	TokenType.AMP:       500, 
	TokenType.PIPE:      500, 
	TokenType.CARET:     500, 
	TokenType.ELLIPSIS: 400, 
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
			return '...'
		elif self == InfixOp.RNGOPEN:
			return '..<'
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
		elif type == TokenType.AMP:
			return InfixOp.AMP
		elif type == TokenType.PIPE:
			return InfixOp.BITOR
		elif type == TokenType.CARET:
			return InfixOp.BITXOR
		elif type == TokenType.ELLIPSIS:
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
	InfixOp.PLUS,
)

RNG_OPS = (
	InfixOp.RNGCLOSED,
	InfixOp.RNGOPEN
)

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

def strBytes(s):
	result = ''
	esc = False
	for c in s[1:-1]:
		if c == '\\':
			esc = True
		elif esc:
			esc = False
			if c == 'n':
				result += '\n'
			elif c == 'r':
				result += '\r'
			elif c == 't':
				result += '\t'
			elif c == '"':
				result += c
			else:
				result += '\\' + c
		else:
			result += c
	
	return bytes(result, 'utf-8')

class ModLevelDeclAST:
	def __init__(self, doccomment, attrs, extern, nameTok, span):
		self.span = span
		self.nameTok = nameTok
		self.name = nameTok.content if nameTok else None
		self.mangledName = self.name
		self.doccomment = doccomment
		self.attrs = attrs
		self.extern = extern
		self.parentScope = None

class ModAST(ModLevelDeclAST):
	def __init__(self, state, doccomment, nameTok, decls, span, name=None):
		super().__init__(doccomment, None, False, nameTok, span)
		if name: self.name = name
		self.span = span
		self.importDecls = []
		self.fnDecls = []
		self.modDecls = []
		self.staticDecls = []
		self.symbolTable = {}
		
		for decl in decls:
			if type(decl) == FnDeclAST:
				self.fnDecls.append(decl)
			elif type(decl) == ModAST:
				self.modDecls.append(decl)
			else:
				raise RuntimeError('unimplemented!')
			
			if decl.name in self.symbolTable:
				logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
			else:
				self.symbolTable[decl.name] = decl

class AttrAST:
	def __init__(self, name, args, span):
		self.span = span
		self.name = name
		self.args = args

class FnDeclAST(ModLevelDeclAST):
	def __init__(self, doccomment, attrs, extern, nameTok, params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(doccomment, attrs, extern, nameTok, span)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cVarArgsSpan = cVarArgsSpan
		self.returnType = returnType
		self.body = body
		self.cconv = CConv.CAESAR
		self.symbolTable = {}
		self.resolvedSymbolType = None

class FnParamAST:
	def __init__(self, name, typeRef, span):
		self.name = name
		self.typeRef = typeRef
		self.span = span
		self.resolvedSymbolType = None

class CVarArgsParamAST:
	def __init__(self, span):
		self.span = span

class ReturnAST:
	def __init__(self, expr, span):
		self.expr = expr
		self.span = span

class LetAST:
	def __init__(self, mut, name, typeRef, expr, span):
		self.mut = mut
		self.name = name
		self.typeRef = typeRef
		self.expr = expr
		self.span = span
		self.resolvedSymbolType = None
		self.doesReturn = False

class TypeRefAST:
	def __init__(self, name, indirectionLevel, span):
		self.name = name
		self.path = [name]
		self.indirectionLevel = indirectionLevel
		self.span = span
		self.resolvedType = None

class AsgnAST:
	def __init__(self, lvalue, rvalue, span):
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.span = span
		self.doesReturn = False

class WhileAST:
	def __init__(self, expr, block, span):
		self.expr = expr
		self.block = block
		self.span = span
		self.parentScope = None
		self.symbolTable = {}

class BreakAST:
	def __init__(self, span):
		self.span = span

class ContinueAST:
	def __init__(self, span):
		self.span = span

class ValueExprAST:
	def __init__(self):
		self.resolvedType = None
		self.doesReturn = False

class StrLitAST(ValueExprAST):
	def __init__(self, value, span):
		super().__init__()
		self.value = strBytes(value)
		self.span = span

class BoolLitAST(ValueExprAST):
	def __init__(self, value, span):
		super().__init__()
		self.value = value
		self.span = span

class IntLitAST(ValueExprAST):
	def __init__(self, strValue, negate, span):
		super().__init__()
		matches = re.match(r"^(0b|0x)?(.+?)(i8|u8|i16|u16|i32|u32|i64|u64|sz|usz)?$", strValue)
		
		base = 10
		if matches[1]:
			base = 2 if matches[1] == '0b' else 16
		
		value = int(matches[2].replace('_', ''), base)
		suffix = matches[3]
		
		self.base = base
		self.value = -value if negate else value
		self.suffix = suffix
		self.span = span

class FloatLitAST(ValueExprAST):
	def __init__(self, strValue, negate, span):
		super().__init__()
		matches = re.match(r"^(0x)?(.+?)(f32|f64)?$", strValue)
		valueStr = matches[2].replace('_', '').lower()
		
		if matches[1]:
			value = float.fromhex(valueStr)
		else:
			value = float(valueStr)
		
		suffix = matches[3]
		
		self.value = -value if negate else value
		self.suffix = suffix
		self.span = span

class TupleLitAST(ValueExprAST):
	def __init__(self, values, span):
		super().__init__()
		self.values = values
		self.span = span

class BlockAST(ValueExprAST):
	def __init__(self, block):
		super().__init__()
		self.exprs = block.list
		self.span = block.span
		self.parentScope = None
		self.symbolTable = {}

class ValueRefAST(ValueExprAST):
	def __init__(self, path, span):
		super().__init__()
		self.path = path
		self.name = path[-1]
		self.span = span

class InfixOpAST(ValueExprAST):
	def __init__(self, l, r, op, span, opSpan):
		super().__init__()
		self.l = l
		self.r = r
		self.op = op
		self.span = span
		self.opSpan = opSpan

class IndexOpAST(ValueExprAST):
	def __init__(self, expr, index, span):
		super().__init__()
		self.expr = expr
		self.index = index
		self.span = span

class DerefAST(ValueExprAST):
	def __init__(self, expr, derefCount, span):
		super().__init__()
		self.expr = expr
		self.derefCount = derefCount
		self.span = span

class AddressAST(ValueExprAST):
	def __init__(self, expr, span):
		super().__init__()
		self.expr = expr
		self.span = span

class CoercionAST(ValueExprAST):
	def __init__(self, expr, typeRef, span):
		super().__init__()
		self.expr = expr
		self.typeRef = typeRef
		self.span = span

class FnCallAST(ValueExprAST):
	def __init__(self, expr, args, span):
		super().__init__()
		self.expr = expr
		self.args = args
		self.span = span

class IfAST(ValueExprAST):
	def __init__(self, expr, ifBlock, elseBlock, span):
		super().__init__()
		self.expr = expr
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
		self.span = span
		self.parentScope = None
		self.symbolTable = {}

class VoidAST(ValueExprAST):
	def __init__(self, span):
		super().__init__()
		self.span = span