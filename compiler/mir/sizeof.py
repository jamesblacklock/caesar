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
		t = self.symbol.type.resolveGenerics(state.ast.genericInc)
		state.appendInstr(Imm(self, IPTR, t.byteSize))
	
	def __str__(self):
		return 'sizeof {}'.format(self.symbol.name)
