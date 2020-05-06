from .ast     import AST
from .ifexpr  import If
from .block   import BlockInfo, Block
from .ctlflow import Break
from .types   import Void
from .scope   import ScopeType
from .ir      import Br, BrIf, Ret, BlockMarker, Raise, getInputInfo, beginBlock

class Loop(AST):
	def __init__(self, block, span):
		super().__init__(span)
		self.block = block
		self.doesBreak = False
		self.doesReturn = False
	
	def analyze(loop, state, implicitType):
		loop.block.loopExpr = loop
		loop.block = state.analyzeNode(loop.block, Void)
		
		loop.doesBreak = loop.block.doesBreak
		loop.doesReturn = loop.block.doesReturn
	
	def writeIR(ast, state):
		inputTypes, inputSymbols = getInputInfo(state)
		
		continueBlock = state.defBlock(inputTypes, True)
		breakBlock = state.defBlock(inputTypes)
		
		state.appendInstr(Br(ast, continueBlock.index))
		beginBlock(state, ast, continueBlock)
		
		state.pushLoopInfo(ast, continueBlock, breakBlock, inputSymbols)
		
		ast.block.writeIR(state)
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			for symbol in inputSymbols:
				offset = state.localOffset(symbol)
				if offset > 0:
					state.appendInstr(Raise(ast, offset))
			state.appendInstr(Br(ast, continueBlock.index))
		
		inputTypes = [i for i in inputTypes]
		inputSymbols = [i for i in inputSymbols]
		for symbol in state.loopInfo.droppedSymbols:
			if symbol in inputSymbols:
				i = inputSymbols.index(symbol)
				inputSymbols.pop(i)
				inputTypes.pop(i)
			else:
				pass # this means the symbol was declared inside the loop
				# assert 0 # this can probably be deleted
		
		breakBlock.inputs = inputTypes
		state.setupLocals(inputTypes, inputSymbols)
		state.appendInstr(BlockMarker(ast, breakBlock.index))
		
		state.popLoopInfo()
	
	def pretty(self, output, indent=0):
		output.write('loop', indent)
		self.block.pretty(output, indent)

class While(AST):
	def __init__(self, expr, block, span):
		super().__init__(span)
		self.expr = expr
		self.block = block
	
	def lower(ast, state):
		ifBlock = Block(BlockInfo(ast.block.exprs, ast.block.span), ScopeType.IF)
		elseBlock = Block(BlockInfo([Break(ast.span)], ast.span), ScopeType.ELSE)
		ifExpr = If(ast.expr, ifBlock, elseBlock, ast.span)
		loopBlock = Block(BlockInfo([ifExpr], ast.span), ScopeType.LOOP)
		loopExpr = Loop(loopBlock, ast.span)
		loopExpr.loweredFrom = ast
		
		return loopExpr
	
	def pretty(self, output, indent=0):
		output.write('while ', indent)
		self.expr.pretty(output)
		self.block.pretty(output, indent)
