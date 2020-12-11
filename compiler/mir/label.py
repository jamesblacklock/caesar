from .mir import MIR
from ..ir import FundamentalType, Static

class Label(MIR):
	def __init__(self, label, resolvedType, data, span):
		super().__init__(span, True)
		self.label = label
		self.type = resolvedType
		self.span = span
		self.staticData = data
	
	def commit(self, state):
		pass
	
	def writeIR(self, state):
		state.staticDefs.append(self.staticData)
		fType = FundamentalType.fromResolvedType(self.type)
		state.appendInstr(Static(self, fType, self.label))
	
	def __str__(self):
		return '$label({})'.format(self.label)
