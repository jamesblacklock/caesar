from .ast         import AST
from ..log        import logError
from ..types      import canCoerce, typesMatch, OwnedType
from ..mir.coerce import Coerce

class AsExpr(AST):
	def __init__(self, expr, typeRef, span):
		super().__init__(span, True)
		self.expr = expr
		self.typeRef = typeRef
	
	def analyze(self, state, implicitType):
		type = state.resolveTypeRef(self.typeRef)
		access = state.analyzeNode(self.expr, type)
		if access == None or access.type == None:
			return None
		
		if not type or typesMatch(type, access.type):
			return access
		
		if access.type.isOwnedType and not type.isOwnedType:
			if typesMatch(type, access.type.baseType):
				if state.scope.allowUnsafe:
					pass
				else:
					logError(state, self.span, 'cannot case from owned to unowned in safe context')
		elif not canCoerce(access.type, type):
			logError(state, self.span, 'cannot coerce from {} to {}'.format(access.type, type))
		
		if access.type.byteSize == type.byteSize and access.type.isFloatType == type.isFloatType:
			access.type = type
			return access
		
		return Coerce(access, type, self.span)
