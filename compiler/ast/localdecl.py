from .ast           import AST, ValueSymbol
from ..mir.access   import SymbolWrite
from ..symbol.local import Local

class CVarArgsParam(AST):
	def __init__(self, span):
		super().__init__(span)

class LocalDecl(ValueSymbol):
	def __init__(self, name, typeRef, mut, span):
		super().__init__(name, typeRef, span)
		self.mut = mut
		self.dropFn = None
	
	def createSymbol(self, state, isParam=True):
		type = None
		if self.typeRef and not isParam:
			type = state.resolveTypeRef(self.typeRef)
		return Local(self, type, isParam)

class FnParam(LocalDecl):
	def __init__(self, name, typeRef, mut, span):
		super().__init__(name, typeRef, mut, span)
		# self.defaultExpr = defaultExpr

class LetDecl(LocalDecl):
	def __init__(self, name, typeRef, mut, expr, span):
		super().__init__(name, typeRef, mut, span)
		self.expr = expr
	
	def createSymbol(self, state):
		return super().createSymbol(state, False)
	
	def analyze(self, state, implicitType):
		symbol = self.createSymbol(state)
		state.decl(symbol)
		
		if self.expr:
			access = SymbolWrite(self.expr, self.span, self.nameSpan)
			access.symbol = symbol
			access.rvalueImplicitType = symbol.type
			state.analyzeNode(access)
		
		if symbol.type or symbol.dropFn:
			symbol.checkDropFn(state)
		