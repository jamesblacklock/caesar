from .ast  import AST
from ..mir import access

class Index(AST):
	def __init__(self, expr, index, span):
		super().__init__(span, True)
		self.expr = expr
		self.index = index

	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)

class Field(AST):
	def __init__(self, expr, path, span):
		super().__init__(span, True)
		self.expr = expr
		self.path = path
	
	def analyze(self, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, self, implicitType)
	