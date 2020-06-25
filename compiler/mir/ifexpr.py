from .mir   import MIR, indent
from ..ir   import getInputInfo, beginBlock, Br, BrIf, Ret, BlockMarker

class If(MIR):
	def __init__(self, access, ifBlock, elseBlock, type, span):
		super().__init__(span, True)
		self.access = access
		self.block = ifBlock
		self.elseBlock = elseBlock
		self.type = type
	
	def checkFlow(self, scope):
		self.access.checkFlow(scope)
		self.block.checkFlow(scope)
		self.elseBlock.ifBranchOuterSymbolInfo = self.block.lastIfBranchOuterSymbolInfo
		self.elseBlock.checkFlow(scope)
	
	def writeIR(self, state):
		self.access.writeIR(state)
		
		inputTypes, inputSymbols = getInputInfo(state)
		inputTypes, inputSymbols = inputTypes[:-1], inputSymbols[:-1]
		
		ifBlock = state.defBlock(inputTypes)
		elseBlock = state.defBlock(inputTypes)
		
		state.appendInstr(BrIf(self, ifBlock.index, elseBlock.index))
		
		beginBlock(state, self.block, ifBlock)
		assert not state.didBreak
		self.block.writeIR(state)
		didBreak = state.didBreak
		state.didBreak = False
		
		endIfBlock = None
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			endInputTypes, endInputNames = getInputInfo(state)
			endIfBlock = state.defBlock(endInputTypes)
			state.appendInstr(Br(self, endIfBlock.index))
		
		state.setupLocals(inputTypes, inputSymbols)
		state.appendInstr(BlockMarker(self.elseBlock, elseBlock.index))
		self.elseBlock.writeIR(state)
		
		state.didBreak = state.didBreak and didBreak
		
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			if endIfBlock == None:
				endInputTypes, endInputNames = getInputInfo(state)
				endIfBlock = state.defBlock(endInputTypes)
			state.appendInstr(Br(self, endIfBlock.index))
		
		if endIfBlock != None:
			# assert not self.doesReturn and not self.doesBreak
			state.setupLocals(endInputTypes, endInputNames)
			beginBlock(state, self, endIfBlock)
	
	def __str__(self):
		return 'if {} {}\nelse {}'.format(str(self.access), str(self.block), str(self.elseBlock))
