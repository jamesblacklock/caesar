from .ast          import AST
from ..types       import typesMatch, Void
from ..log         import logError

def isInLoop(state, span, isContinue):
	if len(state.breakBlocks) == 0:
		logError(state, span, '`{}` expression is not inside a loop'
			.format('continue' if isContinue else 'break'))
		return False
	return True

class Break(AST):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(self, state, implicitType):
		if isInLoop(state, self.span, False):
			state.doBreak()

class Continue(AST):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(self, state, implicitType):
		if isInLoop(state, self.span, True):
			state.doContinue()

class Return(AST):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		expectedReturnType = state.fn.type.returnType
		returnType = Void
		
		access = None
		if self.expr:
			access = state.analyzeNode(self.expr, expectedReturnType)
			if access == None or access.type == None:
				state.scope.didReturn = True
				state.block.didReturn = True
				return
			returnType = access.type
			state.block.outputs.add(access.symbol)
			state.block.returnAccess = access
		
		if expectedReturnType and not typesMatch(returnType, expectedReturnType):
			span = self.expr.span if self.expr else self.span
			logError(state, span, 'invalid return type (expected {}, found {})'
				.format(expectedReturnType, returnType))
		
		state.scope.didReturn = True
		state.block.didReturn = True
