from .mir import MIR
from ..ir import Ret, Br, Raise

class LoopCtl(MIR):
	def __init__(self, isContinue, dropBlock, span):
		super().__init__(span)
		self.isContinue = isContinue
		self.dropBlock = dropBlock
	
	def checkFlow(self, scope):
		scope.doBreak(self, self.isContinue)
	
	def writeIR(self, state):
		state.didBreak = True
		if self.isContinue:
			for symbol in state.loopInfo.inputSymbols:
				offset = state.localOffset(symbol)
				if offset > 0:
					state.appendInstr(Raise(self, offset))
		
		blockDef = state.loopInfo.continueBlock if self.isContinue else state.loopInfo.breakBlock
		state.appendInstr(Br(self, blockDef.index))
	
	def __str__(self):
		return 'continue' if self.isContinue else 'break'

class Return(MIR):
	def __init__(self, access, dropBlock, span):
		super().__init__(span)
		self.access = access
		self.dropBlock = dropBlock
	
	def checkFlow(self, scope):
		if self.access:
			self.access.checkFlow(scope)
		scope.doReturn(self.access.symbol if self.access else None, self)
	
	def writeIR(self, state):
		state.didBreak = True
		if self.access != None:
			self.access.writeIR(state)
		
		if state.retType == None:
			assert len(state.operandStack) == 0
		else:
			assert len(state.operandStack) == 1
		
		state.appendInstr(Ret(self))
	
	def __str__(self):
		if self.access:
			return 'return {}'.format(self.access)
		else:
			return 'return'
