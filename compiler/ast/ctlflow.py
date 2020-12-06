from .ast          import AST
from ..types       import typesMatch, Void
from ..mir.block   import createDropBlock
from ..log         import logError
from ..mir.ctlflow import LoopCtl, Return as ReturnMIR

def analyzeBreakOrContinue(state, expr, isContinue):
	# if state.scope.loopDepth == 0:
	if len(state.breakBlocks) == 0:
		logError(state, expr.span, '`{}` expression is not inside a loop'
			.format('continue' if isContinue else 'break'))
		return None
	
	# state.scope.didBreak = True
	# dropBlock = createDropBlock(expr)
	# state.mirBlock.append(dropBlock)
	# return LoopCtl(isContinue, dropBlock, expr.span)
	if isContinue:
		state.doContinue()
	else:
		state.doBreak()

class Break(AST):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze2(self, state, implicitType):
		return analyzeBreakOrContinue(state, self, False)

class Continue(AST):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze2(self, state, implicitType):
		return analyzeBreakOrContinue(state, self, True)

class Return(AST):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def analyze(self, state, implicitType):
		expectedReturnType = state.scope.fnDecl.type.returnType
		returnType = Void
		
		access = None
		if self.expr:
			access = state.analyzeNode(self.expr, expectedReturnType)
			if access == None:
				return None
			elif access.type:
				returnType = access.type
		
		if not typesMatch(returnType, expectedReturnType):
			span = self.expr.span if self.expr else self.span
			logError(state, span, 'invalid return type (expected {}, found {})'
				.format(expectedReturnType, returnType))
		
		state.scope.didReturn = True
		
		# dropBlock = createDropBlock(self)
		assert state.scope.dropBlock
		dropBlock = state.scope.dropBlock
		state.scope.dropBlock = None
		
		state.mirBlock.append(dropBlock)
		return ReturnMIR(access, dropBlock, self.span)
