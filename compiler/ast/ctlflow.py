from .ast          import AST
from ..types       import typesMatch, Void
from .             import valueref, block
from ..mir         import access
from ..scope       import ScopeType
from ..log         import logError
from ..mir.ctlflow import LoopCtl, Return as ReturnMIR

def analyzeBreakOrContinue(state, expr, isContinue):
	if state.scope.loopDepth == 0:
		logError(state, expr.span, '`{}` expression is not inside a loop'
			.format('continue' if isContinue else 'break'))
	else:
		state.scope.didBreak = True

class Break(AST):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(self, state, implicitType):
		analyzeBreakOrContinue(state, self, False)
		return LoopCtl(False, self.span)

class Continue(AST):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(self, state, implicitType):
		analyzeBreakOrContinue(state, self, True)
		return LoopCtl(True, self.span)

class Return(AST):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		expectedReturnType = state.scope.fnDecl.returnType
		returnType = Void
		
		access = None
		if self.expr:
			access = state.analyzeNode(self.expr, expectedReturnType)
			if access.type:
				returnType = access.type
		
		if not typesMatch(returnType, expectedReturnType):
			span = self.expr.span if self.expr else self.span
			logError(state, span, 'invalid return type (expected {}, found {})'
				.format(expectedReturnType, returnType))
		
		state.scope.didReturn = True
		
		return ReturnMIR(access, self.span)
