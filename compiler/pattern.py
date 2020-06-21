from .ast       import AST, ValueExpr, Contract
from .enumdecl  import VariantDecl
from .log       import logError
from .access    import SymbolRead
from .primitive import IntLit
from .infix     import InfixOp, InfixOps
from .block     import Block
from .token     import Token, TokenType
from .field     import Field

class IsExpr(ValueExpr):
	def __init__(self, expr, pattern, span):
		super().__init__(span)
		self.expr = expr
		self.pattern = pattern
	
	def analyze(self, state, implicitType):
		typeChecked = state.analyzeNode(self.expr)
		if not typeChecked.type:
			return
		
		if not typeChecked.type.isEnumType:
			logError(state, typeChecked.span, 'type `{}` is not an `enum` type'.format(typeChecked.type))
			return
		
		variant = state.lookupSymbol(self.pattern.path, typeChecked.type.symbolTable, inTypePosition=True)
		if variant == None:
			return
		
		assert type(variant) == VariantDecl
		
		if type(self.expr) == Field:
			tagField = self.expr
		else:
			tagField = Field(self.expr, [], self.expr.span)
		tagField.path.append(Token(self.expr.span, TokenType.NAME, '$tag'))
		
		enumType = typeChecked.type
		
		tagValue = IntLit.const(enumType.symbolTable[variant.name].tag.data, enumType.tagType, self.span)
		eq = InfixOp(tagField, tagValue, InfixOps.EQ, None, self.span)
		
		result = state.analyzeNode(eq)
		access = result.l
		if type(access) == Block:
			access = access.exprs[-3].rvalue
		
		result.contracts = { access.symbol: Contract(access.symbol, enumType, [variant], access.deref) }
		
		return result
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write(' is ')
		self.pattern.pretty(output)

class Pattern(AST):
	def __init__(self, path, names, span):
		super().__init__(span)
		self.path = path
		self.names = names
	
	def pretty(self, output, indent=0):
		output.write('<a pattern>', indent)
	