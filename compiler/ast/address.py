from .ast  import AST
from ..mir import access

class Address(AST):
	def __init__(self, expr, mut, span):
		super().__init__(span, True)
		self.expr = expr
		self.mut = mut
	
	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
	