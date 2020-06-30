from .ast  import AST
from ..mir import access

class Deref(AST):
	def __init__(self, expr, count, span):
		super().__init__(span, True)
		self.expr = expr
		self.count = count
	
	def analyze(deref, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, deref, implicitType)
	
		