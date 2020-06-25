from .ast  import ValueExpr
from ..mir import access

class Address(ValueExpr):
	def __init__(self, expr, mut, span):
		super().__init__(span)
		self.expr = expr
		self.mut = mut
	
	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
	