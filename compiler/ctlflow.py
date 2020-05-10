from .ast   import AST
from .types import typesMatch, Void
from .block import Block
from .      import valueref, access
from .scope import ScopeType
from .ir    import Ret, Br, Raise
from .log   import logError

class LoopCtlFlow(AST):
	def __init__(self, span):
		super().__init__(span)
		self.block = None
	
	def analyze(expr, state, implicitType, isContinue=False):
		expr.block = Block([expr], expr.span)
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
	
	def analyze(ret, state, implicitType):
		returnType = Void
		ret.block = Block([], ret.span, noLower=True)
		if ret.expr:
			if type(ret.expr) != valueref.ValueRef:
				(tempSymbol, tempWrite, tempRead) = access.createTempTriple(ret.expr)
				tempWrite.rvalueImplicitType = state.scope.fnDecl.returnType
				ret.expr = tempRead
				ret.block.exprs.extend([
					state.analyzeNode(tempSymbol), 
					state.analyzeNode(tempWrite), 
					ret])
			
			ret.expr = state.analyzeNode(ret.expr)
			returnType = ret.expr.symbol.type
		
		ret.block.exprs.append(ret)
		
		# assignType = getValidAssignType(state.scope.fnDecl.returnType, returnType)
		# if assignType:
		# 	if ret.expr:
		# 		# ret.expr.type = assignType
		# else:
		if not typesMatch(state.scope.fnDecl.returnType, returnType):
			foundType = ret.expr.type if ret.expr else Void
			span = ret.expr.span if ret.expr else ret.span
			logError(state, span, 'invalid return type (expected {}, found {})'
				.format(state.scope.fnDecl.returnType, foundType))
		
		state.scope.doReturn(ret.expr.symbol if ret.expr else None, ret)
		
		return ret.block
	
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
