from .mir import MIR
from ..ir import Raise, Pop

class DropSymbol(MIR):
	def __init__(self, symbol, span):
		super().__init__(span)
		self.symbol = symbol
	
	def commit(self, state):
		pass
	
	def writeIR(expr, state):
		if expr.symbol.type.isVoidType:
			return
		
		if state.loopInfo:
			state.loopInfo.droppedSymbols.add(expr.symbol)
		
		offset = state.localOffset(expr.symbol)
		if offset > 0:
			state.appendInstr(Raise(expr, offset))
		state.appendInstr(Pop(expr))
	
	def __str__(self):
		return '$drop({})'.format(self.symbol.name)
