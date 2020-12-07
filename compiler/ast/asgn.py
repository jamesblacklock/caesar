from .ast   import AST
from .infix import InfixOp
from ..mir  import access

class Asgn(AST):
	def __init__(self, lvalue, rvalue, infixOp, opSpan, span):
		super().__init__(span)
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.infixOp = infixOp
		self.opSpan = opSpan
	
	def analyze(self, state, ignoredImplicitType):
		asgn = self
		if self.infixOp:
			rvalue = InfixOp(self.lvalue, self.rvalue, self.infixOp, self.opSpan, self.span)
			asgn = Asgn(self.lvalue, rvalue, None, self.opSpan, self.span)
		
		return access.SymbolAccess.analyzeSymbolAccess(state, asgn)
