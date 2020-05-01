from .ast   import AST
from .types import getValidAssignType, Void
from .block import Block, BlockInfo
from .scope import ScopeType
from .ir    import Ret, Br
from .log   import logError

class LoopCtlFlow(AST):
	def __init__(self, span):
		super().__init__(span)
		self.dropSymbols = []
		self.block = None
	
	def analyze(expr, state, implicitType):
		if state.scope.loopDepth == 0:
			logError(state, expr.span, '`{}` expression is not inside a loop'
				.format('break' if type(expr) == Break else 'continue'))
		
		symbolInfo = state.scope.symbolInfo
		scope = state.scope
		while True:
			if scope.type == ScopeType.LOOP:
				scope.loopExpr.breaks.append(expr)
				break
			
			for symbol in scope.symbolTable.values():
				if not symbolInfo[symbol].uninit:
					expr.dropSymbols.append(symbol)
			
			scope = scope.parent
		
		expr.block = Block(BlockInfo([expr], expr.span))
		expr.block.type = Void
	
	def writeIR(ast, state, isBreak=False):
		blockDef = state.loopInfo.breakBlock if isBreak else state.loopInfo.continueBlock
		state.appendInstr(Br(ast, blockDef.index))

class Break(LoopCtlFlow):
	def __init__(self, span):
		super().__init__(span)
	
	def writeIR(ast, state):
		super().writeIR(state, True)
	
	def pretty(self, output, indent=0):
		output.write('break', indent)

class Continue(LoopCtlFlow):
	def __init__(self, span):
		super().__init__(span)
	
	def pretty(self, output, indent=0):
		output.write('continue', indent)

class Return(AST):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
		# self.dropSymbols = []
		self.block = None
	
	def lower(ret, state):
		if ret.block:
			return ret
		
		if ret.expr and type(ret.expr) != LetDecl:
			tempSymbol = letdecl.LetDecl(None, None, False, None, None, temp=True)
			# tempSymbol.type = state.scope.fnDecl.returnType
			
			tempLValue = valueref.ValueRef(None, None, temp=True)
			tempLValue.symbol = tempSymbol
			tempAsgn = Asgn(tempLValue, ret.expr, ret.expr.span, temp=True)
			tempAsgn.lowered = True
			
			ret.expr = valueref.ValueRef(None, None, temp=True)
			ret.expr.symbol = tempSymbol
			
			ret.block = Block(BlockInfo([tempSymbol, tempAsgn, ret], ret.span))
		else:
			ret.block = Block(BlockInfo([ret], ret.span))
		
		ret.block.type = Void
		ret.block.lowered = True
		return ret
	
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
		
		# symbolInfo = state.scope.symbolInfo
		# if state.scope.type != ScopeType.FN:
		# 	scope = state.scope.parent
		# 	while True:
		# 		for symbol in scope.symbolTable.values():
		# 			infoScope = state.scope
		# 			while symbol not in infoScope.symbolInfo:
		# 				infoScope = infoScope.parent
		# 			if not infoScope.symbolInfo[symbol].uninit:
		# 				ret.dropSymbols.append(symbol)
				
		# 		if scope.type == ScopeType.FN:
		# 			scope.fnDecl.returns.append(ret)
		# 			break
				
		# 		scope = scope.parent
		
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
