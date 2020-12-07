from .mir    import MIR, StaticData, StaticDataType
from ..ir    import FundamentalType, Imm, I8
from ..types import Bool, Void

class VoidValue(MIR):
	def __init__(self, span):
		super().__init__(span)
		self.type = Void
		self.hasValue = True
	
	def commit(self, state):
		pass
	
	def writeIR(lit, state):
		pass
	
	def __str__(self):
		return 'void'

class BoolValue(MIR):
	def __init__(self, value, span):
		super().__init__(span)
		self.value = value
		self.type = Bool
		self.hasValue = True
	
	def commit(self, state):
		pass
	
	def writeIR(self, state):
		state.appendInstr(Imm(self, I8, 1 if self.value else 0))
	
	def __str__(self):
		return 'true' if self.value else 'false'

class IntValue(MIR):
	def __init__(self, value, type, span):
		super().__init__(span)
		self.value = value
		self.type = type
		self.hasValue = True
	
	def commit(self, state):
		pass
	
	def staticEval(self, state):
		fType = FundamentalType.fromResolvedType(self.type)
		return StaticData(self.value, StaticDataType.INT, fType)
	
	def writeIR(self, state):
		fType = FundamentalType.fromResolvedType(self.type)
		state.appendInstr(Imm(self, fType, self.value))
	
	def __str__(self):
		return str(self.value)

class FloatValue(MIR):
	def __init__(self, value, type, span):
		super().__init__(span, True)
		self.value = value
		self.type = type
	
	def commit(self, state):
		pass
	
	def writeIR(self, state):
		fType = FundamentalType.fromResolvedType(self.type)
		state.appendInstr(Imm(self, fType, self.value))
	
	def __str__(self):
		return str(self.value)
