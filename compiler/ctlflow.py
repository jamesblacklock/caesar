from .ast   import AST
from .types import getValidAssignType, Void
from .block import Block, BlockInfo
from .      import valueref, letdecl, asgn
from .scope import ScopeType
from .ir    import Ret, Br, Raise
from .log   import logError

class LoopCtlFlow(AST):
	def __init__(self, span):
		super().__init__(span)
		self.block = None
	
	def analyze(expr, state, implicitType, isContinue=False):
		expr.block = Block(BlockInfo([expr], expr.span))
		expr.block.type = Void
		
		if state.scope.loopDepth == 0:
			logError(state, expr.span, '`{}` expression is not inside a loop'
				.format('break' if type(expr) == Break else 'continue'))
		else:
			state.scope.doBreak(expr, isContinue)
	
	def writeIR(ast, state, isContinue=False):
		if isContinue:
			for symbol in state.loopInfo.inputSymbols:
				offset = state.localOffset(symbol)
				if offset > 0:
					state.appendInstr(Raise(ast, offset))
		
		blockDef = state.loopInfo.continueBlock if isContinue else state.loopInfo.breakBlock
		state.appendInstr(Br(ast, blockDef.index))

class Break(LoopCtlFlow):
	def __init__(self, span):
		super().__init__(span)
	
	def pretty(self, output, indent=0):
		output.write('break', indent)

class Continue(LoopCtlFlow):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(expr, state, implicitType):
		super().analyze(state, implicitType, True)
	
	def writeIR(ast, state):
		super().writeIR(state, True)
	
	def pretty(self, output, indent=0):
		output.write('continue', indent)

class Return(AST):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
		self.block = None
	
	def lower(ret, state):
		if ret.block:
			return ret
		
		if ret.expr and type(ret.expr) != valueref.ValueRef:
			tempSymbol = letdecl.LetDecl(None, None, False, None, None, temp=True)
			
			tempLValue = valueref.ValueRef(None, None, temp=True)
			tempLValue.symbol = tempSymbol
			tempAsgn = asgn.Asgn(tempLValue, ret.expr, ret.expr.span, temp=True)
			tempAsgn.lowered = True
			
			ret.expr = valueref.ValueRef(None, ret.expr.span, temp=True)
			ret.expr.symbol = tempSymbol
			
			ret.block = Block(BlockInfo([tempSymbol, tempAsgn, ret], ret.span))
		else:
			ret.block = Block(BlockInfo([ret], ret.span))
		
		ret.block.type = Void
		ret.block.lowered = True
		return ret.block
	
	def analyze(ret, state, implicitType):
		returnType = Void
		if ret.expr:
			assert type(ret.expr) == valueref.ValueRef
			# if ret.expr.type == None:
			ret.expr = state.analyzeNode(ret.expr)
			returnType = ret.expr.symbol.type
		
		assignType = getValidAssignType(state.scope.fnDecl.returnType, returnType)
		if assignType:
			if ret.expr:
				ret.expr.type = assignType
		else:
			foundType = ret.expr.type if ret.expr else Void
			span = ret.expr.span if ret.expr else ret.span
			logError(state, span, 'invalid return type (expected {}, found {})'
				.format(state.scope.fnDecl.returnType, foundType))
		
		state.scope.doReturn(ret.expr.symbol if ret.expr else None, ret)
	
	def writeIR(ast, state):
		if ast.expr != None:
			ast.expr.writeIR(state)
		
		if state.retType == None:
			assert len(state.operandStack) == 0
		else:
			assert len(state.operandStack) == 1
		
		state.appendInstr(Ret(ast))
	
	def pretty(self, output, indent=0):
		output.write('return', indent)
		if self.expr:
			output.write(' ')
			self.expr.pretty(output)
