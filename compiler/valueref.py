from .ast      import ValueExpr
from .access   import SymbolAccess
from .         import letdecl, staticdecl

class ValueRef(ValueExpr):
	def __init__(self, path, span):
		super().__init__(span)
		self.path = path
	
	def analyze(valueRef, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, valueRef, implicitType)
	
	def pretty(self, output, indent=0):
		path = '::'.join(tok.content for tok in self.path)
		output.write(path, indent)

class Borrow(ValueExpr):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
	
	def pretty(self, output, indent=0):
		output.write('borrow ', indent)
		self.expr.pretty(output)
