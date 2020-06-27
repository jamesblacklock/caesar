from .ast      import AST
from ..mir.neg import Neg

class Sign(AST):
	def __init__(self, expr, negate, span):
		super().__init__(span, True)
		self.expr = expr
		self.negate = negate

	def analyze(self, state):
		access = state.analyzeNode(self.expr)
		if not access or access.type == None:
			return access
		elif not access.type.isSigned:
			logError(state, self.expr.span, 'type `{}` has no sign'.format(access.type.name))
		elif not self.negate:
			return access
		else:
			return Neg(access, access.span)
