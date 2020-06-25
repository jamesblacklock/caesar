from .ast              import AST, ValueSymbol
from ..mir             import access as accessmod
from ..mir.localsymbol import LocalSymbol

class CVarArgsParam(AST):
	def __init__(self, span):
		super().__init__(span)

class LocalDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, mut, span):
		super().__init__(nameTok, typeRef, span)
		self.mut = mut
	
	def analyze(self, state, implicitType, dropBlock, isParam):
		type = None
		if self.typeRef:
			type = state.resolveTypeRef(self.typeRef)
		
		name = None if self.name == '_' else self.name
		symbol = LocalSymbol(name, type, self.mut, isParam, self.span)
		symbol.dropBlock = dropBlock
		state.scope.declSymbol(symbol)
		
		return symbol

class FnParam(LocalDecl):
	def __init__(self, nameTok, typeRef, span):
		super().__init__(nameTok, typeRef, False, span)
		# self.defaultExpr = defaultExpr
		self.symbol = None
	
	def analyze(self, state, implicitType):
		self.symbol = super().analyze(state, implicitType, state.scope.fnDecl.paramDropBlock, True)

class LetDecl(LocalDecl):
	def __init__(self, nameTok, typeRef, mut, expr, span):
		super().__init__(nameTok, typeRef, mut, span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		symbol = super().analyze(state, implicitType, state.scope.dropBlock, False)
		state.mirBlock.append(symbol)
		result = None
		if self.expr:
			access = accessmod.SymbolWrite(self.expr, self.span, self.nameTok.span)
			access.symbol = symbol
			access.rvalueImplicitType = symbol.type
			state.analyzeNode(access)
		
		if symbol.type:
			symbol.checkDropFn(state)