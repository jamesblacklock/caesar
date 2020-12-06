from .ast         import AST
from ..log        import logError
from ..types      import canCoerce, typesMatch, OwnedType
from ..mir.coerce import Coerce

class AsExpr(AST):
	def __init__(self, expr, typeRef, span):
		super().__init__(span, True)
		self.expr = expr
		self.typeRef = typeRef
	
	def analyze2(self, state, implicitType):
		type = state.resolveTypeRef(self.typeRef)
		access = state.analyzeNode2(self.expr, type)
		if access == None or access.type == None:
			return None
		
		if not type or typesMatch(type, access.type):
			return access
		elif access.type.isOwnedType and not type.isOwnedType:
			t = access.type
			type = OwnedType(type, t.acquire, t.release, t.acquireSpan, t.releaseSpan, type.span)
		
		if not canCoerce(access.type, type):
			logError(state, self.span, 'cannot coerce from {} to {}'.format(access.type, type))
		
		if access.type.byteSize == type.byteSize and access.type.isFloatType == type.isFloatType:
			access.type = type
			return access
		
		return Coerce(access, type, self.span)
