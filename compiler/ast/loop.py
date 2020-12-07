from .ast       import AST
from .ifexpr    import If
from .block     import Block
from .ctlflow   import Break

class Loop(AST):
	def __init__(self, block, span):
		super().__init__(span)
		self.block = block
	
	def analyze(self, state, implicitType):
		state.beginScope(self.block.span, loop=True)
		self.block.hasScope = False
		state.analyzeNode(self.block)
		state.endScope()

class While(AST):
	def __init__(self, expr, block, span):
		super().__init__(span)
		self.expr = expr
		self.block = block
	
	def analyze(self, state, implicitType):
		ifBlock = Block(self.block.exprs, self.block.span)
		elseBlock = Block([Break(self.span)], self.span.endSpan())
		ifExpr = If(self.expr, ifBlock, elseBlock, self.block.span)
		loopBlock = Block([ifExpr], self.span)
		state.analyzeNode(Loop(loopBlock, self.span))
