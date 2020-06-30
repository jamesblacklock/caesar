from .ast              import AST, Contract
from ..infixops        import InfixOps
from ..symbol.enumdecl import VariantDecl
from ..log             import logError
from ..mir.primitive   import IntValue
from .infix            import InfixOp
from .field            import Field

class IsExpr(AST):
	def __init__(self, expr, pattern, span):
		super().__init__(span, True)
		self.expr = expr
		self.pattern = pattern
	
	def analyze(self, state, implicitType):
		typeChecked = state.analyzeNode(self.expr, discard=True)
		if not typeChecked or not typeChecked.type:
			return None
		
		if not typeChecked.type.isEnumType:
			logError(state, typeChecked.span, 'type `{}` is not an `enum` type'.format(typeChecked.type))
			return None
		
		enumType = typeChecked.type
		variant = state.lookupSymbol(self.pattern.path, enumType.symbolTable, inTypePosition=True)
		if variant == None:
			return None
		
		assert type(variant) == VariantDecl
		
		if type(self.expr) == Field:
			tagField = self.expr
		else:
			tagField = Field(self.expr, [], self.expr.span)
		
		class T:
			def __init__(content, span):
				self.content = content
				self.span = span
		tagField.path.append(T('$tag', self.expr.span))
		
		
		tagValue = enumType.symbolTable[variant.name].tag.data
		tagExpr = IntValue(tagValue, enumType.tagType, self.span)
		eq = InfixOp(tagField, tagExpr, InfixOps.EQ, None, self.span)
		
		access = state.analyzeNode(eq)
		if access:
			access.contracts = { typeChecked.symbol: Contract(typeChecked.symbol, enumType, [variant], typeChecked.deref) }
		
		return access

class Pattern(AST):
	def __init__(self, path, names, span):
		super().__init__(span)
		self.path = path
		self.names = names
	