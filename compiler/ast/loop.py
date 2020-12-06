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
	
	def analyze2(self, state, implicitType):
		state.beginScope(self.block.span, loop=True)
		self.block.hasScope = False
		state.analyzeNode(self.block, Void)
		state.endScope()

class While(AST):
	def __init__(self, expr, block, span):
		super().__init__(span)
		self.expr = expr
		self.block = block
	
	def analyze2(self, state, implicitType):
		ifBlock = Block(self.block.exprs, ScopeType.IF, self.block.span)
		elseBlock = Block([Break(self.span)], ScopeType.ELSE, self.span.endSpan())
		ifExpr = If(self.expr, ifBlock, elseBlock, self.block.span)
		loopBlock = Block([ifExpr], ScopeType.LOOP, self.span)
		state.analyzeNode(Loop(loopBlock, self.span), Void)
