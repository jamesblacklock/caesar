from .ast    import ValueExpr
from .       import access

class Address(ValueExpr):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
	
	def pretty(self, output, indent=0):
		output.write('&', indent)
		self.expr.pretty(output)