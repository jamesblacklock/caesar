from .ast         import ValueExpr
from ..log        import logError
from ..types      import canCoerce, typesMatch, OwnedType
from ..mir.coerce import Coerce

class AsExpr(ValueExpr):
	def __init__(self, expr, typeRef, span):
		super().__init__(span)
		self.expr = expr
		self.typeRef = typeRef
	
	def analyze(self, state, implicitType):
		type = state.resolveTypeRef(self.typeRef)
		access = state.analyzeNode(self.expr, type)
		
		if not type or typesMatch(type, access.type):
			return access
		elif access.type.isOwnedType and not type.isOwnedType:
			t = access.type
			type = OwnedType(type, t.acquire, t.release, t.acquireSpan, t.releaseSpan)
		
		if not canCoerce(access.type, type):
			logError(state, self.span, 'cannot coerce from {} to {}'.format(access.type, type))
		
		if access.type.byteSize == type.byteSize and access.type.isFloatType == type.isFloatType:
			access.type = type
			return access
		
		return Coerce(access, type, self.span)
