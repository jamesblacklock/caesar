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
