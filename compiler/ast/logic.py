from .ast            import AST
from ..              import types
from ..infixops      import InfixOps
from ..symbol.local  import Local
from ..mir.access    import SymbolWrite, SymbolRead
from ..mir.primitive import BoolValue
from ..mir.block     import createDropBlock
from ..scope         import ScopeType
from ..mir.ifexpr    import If as IfMIR
from ..log           import logError

class LogicOp(AST):
	def __init__(self, l, r, op, opSpan, span):
		super().__init__(span, True)
		self.l = l
		self.r = r
		self.op = op
		self.opSpan = opSpan
	
	def generateRighthand(self, state, symbol):
		r = state.analyzeNode(self.r, types.Bool)
		if r:
			r = state.typeCheck(r, types.Bool)
			write = SymbolWrite(r, self.r.span)
			write.symbol = symbol
			state.analyzeNode(write)
	
	def generateShortCircuit(self, state, symbol, value):
		write = SymbolWrite(BoolValue(value, self.span), self.span)
		write.symbol = symbol
		state.analyzeNode(write)
	
	def analyze(self, state, implicitType):
		isAnd = self.op == InfixOps.AND
		symbol = Local.createTemp(self.span)
		symbol.declSymbol(state.scope)
		
		l = state.analyzeNode(self.l, types.Bool)
		if l:
			l = state.typeCheck(l, types.Bool)
		
		state.pushScope(ScopeType.IF)
		state.scope.dropBlock = createDropBlock(self)
		if isAnd:
			self.generateRighthand(state, symbol)
		else:
			self.generateShortCircuit(state, symbol, True)
		ifBlock = state.popScope()
		
		state.pushScope(ScopeType.ELSE)
		state.scope.dropBlock = createDropBlock(self)
		if isAnd:
			self.generateShortCircuit(state, symbol, False)
		else:
			self.generateRighthand(state, symbol)
		elseBlock = state.popScope()
		
		didReturn = ifBlock.scope.didReturn and elseBlock.scope.didReturn
		state.scope.didReturn = state.scope.didReturn or didReturn
		
		didBreak = ifBlock.scope.didBreak and elseBlock.scope.didBreak
		state.scope.didBreak = state.scope.didBreak or didBreak
		
		mir = IfMIR(l, ifBlock, elseBlock, types.Bool, self.span)
		state.mirBlock.append(mir)
		
		result = SymbolRead(self.span)
		result.symbol = symbol
		result.type = types.Bool
		result.ref = True
		return result
