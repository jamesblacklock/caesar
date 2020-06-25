from .mir import MIR, indent
from ..ir import Br, BrIf, Ret, BlockMarker, Raise, getInputInfo, beginBlock

class Loop(MIR):
	def __init__(self, block, span):
		super().__init__(span)
		self.block = block
	
	def writeIR(self, state):
		inputTypes, inputSymbols = getInputInfo(state)
		
		continueBlock = state.defBlock(inputTypes, True)
		breakBlock = state.defBlock(inputTypes)
		
		state.appendInstr(Br(self, continueBlock.index))
		beginBlock(state, self, continueBlock)
		
		state.pushLoopInfo(self, continueBlock, breakBlock, inputSymbols)
		
		self.block.writeIR(state)
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			for symbol in inputSymbols:
				offset = state.localOffset(symbol)
				if offset > 0:
					state.appendInstr(Raise(self, offset))
			state.appendInstr(Br(self, continueBlock.index))
		
		inputTypes = [i for i in inputTypes]
		inputSymbols = [i for i in inputSymbols]
		for symbol in state.loopInfo.droppedSymbols:
			if symbol in inputSymbols:
				i = inputSymbols.index(symbol)
				inputSymbols.pop(i)
				inputTypes.pop(i)
			else:
				pass # this means the symbol was declared inside the loop
				# assert 0 # this can probably be deleted
		
		breakBlock.inputs = inputTypes
		state.setupLocals(inputTypes, inputSymbols)
		state.appendInstr(BlockMarker(self, breakBlock.index))
		
		state.popLoopInfo()
	
	def checkFlow(self, scope):
		self.block.checkFlow(scope)
	
	def __str__(self):
		return 'loop {}'.format(indent(str(self.block)))
