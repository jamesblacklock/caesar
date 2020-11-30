from .mir import MIR
from ..ir import FNeg, Neg as INeg

class Neg(MIR):
	def __init__(self, access, span):
		super().__init__(span, True)
		self.access = access
		self.type = access.type
	
	def checkFlow(self, scope):
		self.access.checkFlow(scope)
	
	def writeIR(self, state):
		self.access.writeIR(state)
		if self.type.isFloatType:
			state.appendInstr(FNeg(self))
		else:
			state.appendInstr(INeg(self))
	
	def __str__(self):
		return '-{}'.format(str(self.access))