from .ast              import AST, Contract
from ..infixops        import InfixOps
from ..symbol.enumdecl import VariantDecl
from ..log             import logError
from ..mir.primitive   import IntValue
from ..mir.infix       import InfixOp
from .field            import Field
from ..types           import Bool

class IsExpr(AST):
	def __init__(self, expr, pattern, span):
		super().__init__(span, True)
		self.expr = expr
		self.pattern = pattern
	
	def analyze(self, state, implicitType):
		access = state.analyzeNode(self.expr)#, discard=True)
		if not access or not access.type:
			return None
		
		if not access.type.isEnumType:
			logError(state, access.span, 'type `{}` is not an `enum` type'.format(access.type))
			return None
		
		enumType = access.type
		variant = state.lookupSymbol(self.pattern.path, enumType.symbolTable, inTypePosition=True)
		if variant == None:
			return None
		
		# assert type(variant) == VariantDecl
		
		# if type(self.expr) == Field:
		# 	tagField = self.expr
		# else:
		# 	tagField = Field(self.expr, [], self.expr.span)
		
		# tagField.path.append(T('$tag', self.expr.span))
		tagField = enumType.structType.fields[1]
		access.ref = False
		access.isFieldAccess = True
		access.staticOffset += tagField.offset
		access.type = tagField.type
		
		tagValue = enumType.symbolTable[variant.name].tag.data
		tagExpr = IntValue(tagValue, enumType.tagType, self.span)
		eq = InfixOp(access, tagExpr, InfixOps.EQ, Bool, self.span)
		
		result = state.analyzeNode(eq)
		if result:
			result.contracts = { access.symbol: Contract(access.symbol, enumType, [variant], access.deref) }
		
		return result

class Pattern(AST):
	def __init__(self, path, names, span):
		super().__init__(span)
		self.path = path
		self.names = names
	