from .ast       import AST
from .ifexpr    import If
from .block     import Block
from .ctlflow   import Break
from ..types    import Void
from ..scope    import ScopeType
from ..mir.loop import Loop as LoopMIR

class Loop(AST):
	def __init__(self, block, span):
		super().__init__(span)
		self.block = block
	
	def analyze(loop, state, implicitType):
		# loop.block.loopExpr = loop
		state.pushScope(ScopeType.LOOP, loopExpr=self)
		state.analyzeNode(loop.block, Void)
		block = state.popScope()
		return LoopMIR(block, self.span)

class While(AST):
	def __init__(self, expr, block, span):
		super().__init__(span)
		self.expr = expr
		self.block = block
	
	def analyze(self, state, implicitType):
		ifBlock = Block(self.block.exprs, self.block.span, ScopeType.IF)
		elseBlock = Block([Break(self.span)], self.span, ScopeType.ELSE)
		ifExpr = If(self.expr, ifBlock, elseBlock, self.span)
		loopBlock = Block([ifExpr], self.span, ScopeType.LOOP)
		return state.analyzeNode(Loop(loopBlock, self.span), implicitType)
