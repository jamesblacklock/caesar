from .ast    import AST
from ..mir   import access
from ..scope import ScopeType
from ..log   import logError
from ..      import ir

class Deref(AST):
	def __init__(self, expr, count, span):
		super().__init__(span, True)
		self.expr = expr
		self.count = count
	
	def analyze(deref, state, implicitType):
		return access.SymbolAccess.analyzeSymbolAccess(state, deref, implicitType)
	
		