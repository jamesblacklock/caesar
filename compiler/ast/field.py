from .ast  import AST
from ..mir import access

class Index(AST):
	def __init__(self, expr, index, span):
		super().__init__(span, True)
		self.expr = expr
		self.index = index

	def analyze(expr, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, expr, implicitType)

class Field(AST):
	def __init__(self, expr, path, span):
		super().__init__(span, True)
		self.expr = expr
		self.path = path
	
	def analyze(expr, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, expr, implicitType)
	