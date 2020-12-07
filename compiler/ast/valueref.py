from .ast  import AST
from ..mir import access

class ValueRef(AST):
	def __init__(self, path, span):
		super().__init__(span, True)
		self.path = path
	
	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)

class Borrow(AST):
	def __init__(self, expr, span):
		super().__init__(span, True)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
