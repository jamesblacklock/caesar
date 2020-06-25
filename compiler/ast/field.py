from .ast  import ValueExpr
from ..mir import access

class Index(ValueExpr):
	def __init__(self, expr, index, span):
		super().__init__(span)
		self.expr = expr
		self.index = index

	def analyze(expr, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, expr, implicitType)
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		if self.deref:
			output.write('^')
		output.write('[')
		self.index.pretty(output)
		output.write(']')

class Field(ValueExpr):
	def __init__(self, expr, path, span):
		super().__init__(span)
		self.expr = expr
		self.path = path
	
	def analyze(expr, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, expr, implicitType)
	