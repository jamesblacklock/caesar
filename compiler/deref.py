from .ast   import ValueExpr
from .      import access
from .scope import ScopeType
from .log   import logError
from .      import ir

class Deref(ValueExpr):
	def __init__(self, expr, count, span):
		super().__init__(span)
		self.expr = expr
		self.count = count
	
	def analyze(deref, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, deref, implicitType)
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write('^' * self.count)
		