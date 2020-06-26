from .ast         import AST
from ..mir.access import SymbolAccess

class ValueRef(AST):
	def __init__(self, path, span):
		super().__init__(span, True)
		self.path = path
	
	def analyze(valueRef, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, valueRef, implicitType)

class Borrow(AST):
	def __init__(self, expr, span):
		super().__init__(span, True)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
