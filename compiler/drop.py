from .ast import AST
from .ir  import Raise, Pop

class DropSymbol(AST):
	def __init__(self, symbol):
		super().__init__(None)
		self.symbol = symbol
	
	def analyze(expr, state, implicitType):
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
	
	def pretty(self, output, indent=0):
		output.write('$drop(', indent)
		output.write(self.symbol.name)
		output.write(')')