from .ast         import ValueExpr
from ..log        import logError
from ..types      import canCoerce, typesMatch, OwnedType
from ..mir.coerce import Coerce

class AsExpr(ValueExpr):
	def __init__(self, expr, typeRef, span):
		super().__init__(span)
		self.expr = expr
		self.typeRef = typeRef
	
	def analyze(asExpr, state, implicitType):
		type = state.resolveTypeRef(asExpr.typeRef)
		access = state.analyzeNode(asExpr.expr, asExpr.type)
		
		if not type or typesMatch(type, access.type):
			return access
		elif access.type.isOwnedType and not type.isOwnedType:
			t = asExpr.expr.type
			type = OwnedType(type, t.acquire, t.release, t.acquireSpan, t.releaseSpan)
		
		if not canCoerce(access.type, type):
			logError(state, asExpr.span, 'cannot coerce from {} to {}'.format(access.type, type))
		
		if access.type.byteSize == type.byteSize and access.type.isFloatType == type.isFloatType:
			access.type = type
			return access
		
		return Coerce(access, type, self.span)
