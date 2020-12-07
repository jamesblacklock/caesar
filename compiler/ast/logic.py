from .ast            import AST
from ..              import types
from ..infixops      import InfixOps
from ..symbol.local  import Local
from ..mir.access    import SymbolAccess
from ..mir.primitive import BoolValue
from ..log           import logError

class LogicOp(AST):
	def __init__(self, l, r, op, opSpan, span):
		super().__init__(span, True)
		self.l = l
		self.r = r
		self.op = op
		self.opSpan = opSpan
	
	def generateRighthand(self, state, symbol):
		result = state.analyzeNode(self.r, types.Bool)
		if result:
			result = state.typeCheck(result, types.Bool)
			result = SymbolAccess.write(state, symbol, result)
			state.appendDropPoint()
		state.block.outputs.add(symbol)
		return result
	
	def generateShortCircuit(self, state, symbol, value):
		result = SymbolAccess.write(state, symbol, BoolValue(value, self.span))
		state.appendDropPoint()
		state.block.outputs.add(symbol)
		return result
	
	def analyze(self, state, implicitType):
		isAnd = self.op == InfixOps.AND
		symbol = Local.createTemp(self.span)
		state.decl(symbol)
		
		l = state.analyzeNode(self.l, types.Bool)
		state.appendDropPoint()
		if l:
			l = state.typeCheck(l, types.Bool)
		
		(ifBranch, elseBranch) = state.ifBranch(l.symbol if l else None, self.span)
		
		self.hasValue = True
		
		state.beginScope(self.l.span, ifBranch)
		if isAnd:
			ifAccess = self.generateRighthand(state, symbol)
		else:
			ifAccess = self.generateShortCircuit(state, symbol, True)
		endIfBranch = state.endScope()
		
		state.beginScope(self.l.span, elseBranch)
		if isAnd:
			elseAccess = self.generateShortCircuit(state, symbol, False)
		else:
			elseAccess = self.generateRighthand(state, symbol)
		endElseBranch = state.endScope()
		
		state.endBranch([endIfBranch, endElseBranch], self.span)
		
		didReturn = endIfBranch.didReturn and endElseBranch.didReturn
		state.scope.didReturn = state.scope.didReturn or didReturn
		
		didBreak = endIfBranch.didBreak and endElseBranch.didBreak
		state.scope.didBreak = state.scope.didBreak or didBreak
		
		if endIfBranch.didReturn or endIfBranch.didBreak:
			return elseAccess
		else:
			return ifAccess
