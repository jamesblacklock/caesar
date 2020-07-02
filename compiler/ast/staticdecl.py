from .ast            import ValueSymbol
from ..types         import typesMatch, Void
from ..log           import logError
from ..symbol.static import Static

class StaticDecl(ValueSymbol):
	def __init__(self, name, typeRef, doccomment, extern, mut, expr, span):
		super().__init__(name, typeRef, span, doccomment, extern)
		self.mangledName = None
		self.mut = mut
		self.expr = expr
		
	def createSymbol(self, state, isConst=False):
		if self.name == '_':
			logError(state, self.expr.span, '`_` is not a valid symbol name')
		
		return Static(self, isConst)

class ConstDecl(StaticDecl):
	def __init__(self, name, typeRef, doccomment, expr, span):
		super().__init__(name, typeRef, doccomment, False, False, expr, span)
	
	def createSymbol(self, state):
		return super().createSymbol(state, isConst=True)
	
	def __str__(self):
		return super().__str__('const')
