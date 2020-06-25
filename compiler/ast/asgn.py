from .ast  import AST
from ..mir import access

class Asgn(AST):
	def __init__(self, lvalue, rvalue, infixOp, opTok, span):
		super().__init__(span)
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.infixOp = infixOp
		self.opTok = opTok
	
	def analyze(self, state, ignoredImplicitType):
		if self.infixOp:
			self.rvalue = InfixOp(self.lvalue, self.rvalue, self.infixOp, self.opTok, self.span)
		
		return access.SymbolAccess.analyzeSymbolAccess(state, self)
