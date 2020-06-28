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
	
	def analyze(self, state, implicitType):
		state.pushScope(ScopeType.LOOP, self)
		state.analyzeNode(self.block, Void)
		block = state.popScope()
		return LoopMIR(block, self.span)

class While(AST):
	def __init__(self, expr, block, span):
		super().__init__(span)
		self.expr = expr
		self.block = block
	
	def analyze(self, state, implicitType):
		ifBlock = Block(self.block.exprs, ScopeType.IF, self.block.span)
		elseBlock = Block([Break(self.span)], ScopeType.ELSE, self.span)
		ifExpr = If(self.expr, ifBlock, elseBlock, self.span)
		loopBlock = Block([ifExpr], ScopeType.LOOP, self.span)
		state.analyzeNode(Loop(loopBlock, self.span), Void)
