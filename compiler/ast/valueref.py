from .ast         import ValueExpr
from ..mir.access import SymbolAccess

class ValueRef(ValueExpr):
	def __init__(self, path, span):
		super().__init__(span)
		self.path = path
	
	def analyze(valueRef, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, valueRef, implicitType)

class Borrow(ValueExpr):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
