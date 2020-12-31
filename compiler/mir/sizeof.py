from .mir    import MIR
from ..ir    import IPTR, Imm
from ..types import USize

class GenericSizeof(MIR):
	def __init__(self, symbol, span):
		super().__init__(span, True)
		self.symbol = symbol
		self.span = span
		self.type = USize
	
	def commit(self, state):
		pass
	
	def writeIR(self, state):
		symbol = state.ast.genericInc[self.symbol]
		assert symbol.type.byteSize != None
		state.appendInstr(Imm(self, IPTR, symbol.type.byteSize))
	
	def __str__(self):
		return '$label({})'.format(self.label)
