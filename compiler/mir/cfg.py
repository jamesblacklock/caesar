from .mir    import MIR
from .access import SymbolAccess
from .drop   import DropSymbol
from ..ir    import Br, BrIf, Ret, BlockMarker, getInputInfo

class SymbolState:
	def __init__(self, symbol):
		self.symbol = symbol
		self.wasDeclared = True
		self.wasRedeclared = False
		self.wasTouched = True
		self.wasRead = False
		self.wasWritten = False
		self.lastUse = None
		self.unused = True
		self.init = symbol.isParam
		self.moved = False
	
	def clone(self):
		other = SymbolState(self.symbol)
		other.wasDeclared = False
		other.wasRedeclared = False
		other.wasTouched = False
		other.wasRead = False
		other.wasWritten = False
		other.lastUse = None
		other.unused = self.unused
		other.init = self.init
		other.moved = self.moved
		return other
	
	def recordTouch(self, access):
		if self.wasTouched == False and access.write:
			self.wasRedeclared = True
		
		self.unused = False
		self.wasTouched = True
		self.wasRead = access.ref
		self.wasWritten = access.write
		self.lastUse = access

CFG_BLOCK_COUNT = 0

class CFGBlock:
	def __init__(self, ancestors, span):
		global CFG_BLOCK_COUNT
		CFG_BLOCK_COUNT += 1
		
		self.id = CFG_BLOCK_COUNT
		self.span = span
		self.inputs = set()
		self.decls = set()
		self.outputs = set()
		self.mir = []
		self.symbolState = {}
		self.branchOn = None
		self.ancestors = []
		self.successors = []
		self.finalizeCount = 0
		self.finalized = False
		self.irBlockDef = None
		self.irInputSymbols = None
		self.didReturn = False
		self.didBreak = False
		self.unreachable = False
		
		if ancestors:
			self.ancestors = ancestors
			self.symbolState = { k: v.clone() for (k, v) in ancestors[0].symbolState.items() }
			for ancestor in ancestors:
				ancestor.successors.append(self)
	
	def addReverseAncestor(self, ancestor):
		self.ancestors.append(ancestor)
		ancestor.successors.append(self)
		ancestor.symbolState.update({ k: v.clone() for (k, v) in self.symbolState.items() })
	
	def addAncestor(self, ancestor):
		self.ancestors.append(ancestor)
		ancestor.successors.append(self)
		self.symbolState.update({ k: v.clone() for (k, v) in ancestor.symbolState.items() })
	
	def append(self, mir):
		self.mir.append(mir)
	
	def decl(self, symbol):
		self.decls.add(symbol)
		self.symbolState[symbol] = SymbolState(symbol)
	
	def access(self, access):
		if not (access.symbol and access.symbol.type):
			return
		
		if access.symbol not in self.symbolState:
			assert not access.symbol.isLocal
			return
		
		info = self.symbolState[access.symbol]
		
		if access.write:
			if access.deref:
				self.derefWriteSymbol(access, info)
			else:
				self.writeSymbol(access, info)
		elif access.addr:
			self.addrSymbol(access, info)
		else:
			self.readSymbol(access, info)
		
		info.recordTouch(access)
		if info.wasRedeclared:
			self.decls.add(access.symbol)
		elif not info.wasDeclared:
			self.inputs.add(access.symbol)
	
	def writeSymbol(self, access, info):
		if info.init and not info.moved and info.lastUse:
			self.dropLastUse(info)
		
		info.init = True
		info.moved = False
	
	def readSymbol(self, access, info):
		symbol = access.symbol
		
		if not info.init:
			if info.moved:
				logError(self, access.span, 'the value in `{}` has been moved'.format(symbol.name))
				for use in info.lastUses:
					if use.ref:
						logExplain(self.state, use.span, '`{}` was moved here'.format(symbol.name))
			else:
				logError(self, access.span, '`{}` has not been initialized'.format(symbol.name))
		
		info.moved = not access.copy
	
	def dropLastUse(self, info):
		if info.lastUse.ref and info.lastUse.copy:
			assert not info.lastUse.symbol.dropFn
			info.lastUse.copy = False
		else:
			self.dropSymbol(info.symbol, info.lastUse.dropPoint)
	
	def dropSymbol(self, symbol, dropPoint):
		if symbol.type.isOwnedType:
			logError(self.state, symbol.span, 'owned value was not discarded')
			logExplain(self.state, dropPoint.span.endSpan(), 'value escapes here')
		
		# if symbol.dropFn:
		# 	self.callDropFn(symbol.dropFn, symbol, None, 0, exprs, dropPoint.span)
		
		# self.dropFields(symbol, None, 0, exprs, dropPoint.span)
		
		dropPoint.append(DropSymbol(symbol, dropPoint.span))
		# info.recordDrop(dropPoint)
	
	def finalize(self, successorSymbols=set()):
		assert not self.unreachable
		if self.finalized:
			return
		
		self.finalizeCount += 1
		self.finalized = self.finalizeCount >= len(self.successors)
		finalized = self.finalized
		
		self.outputs.update(successorSymbols)
		self.inputs.update(symbol for symbol in self.outputs if not self.symbolState[symbol].wasTouched)
		for block in self.ancestors:
			block.finalize(self.inputs)
		
		if finalized != self.finalized:
			return
		
		if self.finalized:
			for block in self.successors:
				# assert block.inputs == self.outputs
				dropInBlock = self.outputs - block.inputs
				if dropInBlock:
					block.inputs.update(dropInBlock)
					dropPoint = CFGDropPoint(block.span)
					for symbol in dropInBlock:
						block.dropSymbol(symbol, dropPoint)
					block.mir.insert(0, dropPoint)
			
			for info in self.symbolState.values():
				if info.init and not info.moved and info.lastUse and info.symbol != self.branchOn and not (info.symbol in self.outputs):
					self.dropLastUse(info)
			# 	for block in reversed(self.fnBlocks):
			# 		info = block.symbolState[symbol]
					
			# 		if wasTouchedLater:
			# 			block.outputs.append(symbol)
					
			# 		if info.wasRedeclared:
			# 			block.decls.append(symbol)
			# 			wasTouchedLater = False
			# 			continue
			# 		elif info.wasDeclared:
			# 			if info.init and not info.moved:
			# 				for use in lastUses:
			# 					if use.ref and use.copy:# and not use.symbol.dropFn
			# 						use.copy = False
			# 					else:
			# 						self.block.dropSymbol(info, use.dropPoint)
			# 			break
					
			# 		wasTouchedLater = wasTouchedLater or info.wasTouched
			# 		if wasTouchedLater:
			# 			block.inputs.append(symbol)
	
	def writeIR(self, state):
		assert not self.unreachable
		
		if self.ancestors:
			assert self.irBlockDef
			state.setupLocals(self.irBlockDef.inputs, self.irInputSymbols)
			state.appendInstr(BlockMarker(self, self.irBlockDef.index))
		
		for mir in self.mir:
			mir.writeIR(state)
		
		inputTypes, inputSymbols = getInputInfo(state)
		if self.successors:
			if self.branchOn:
				inputTypes.pop()
				inputSymbols.pop()
			
			for block in self.successors:
				if block.irBlockDef == None:
					block.irBlockDef = state.defBlock(inputTypes)
					block.irInputSymbols = inputSymbols
			
			if self.branchOn:
				assert len(self.successors) == 2
				state.appendInstr(BrIf(self, self.successors[1].irBlockDef.index, self.successors[0].irBlockDef.index))
			else:
				assert len(self.successors) == 1
				state.appendInstr(Br(self, self.successors[0].irBlockDef.index))
		else:
			assert self.didReturn
			# state.didBreak = True
			
			if state.retType == None:
				assert len(state.operandStack) == 0
			else:
				assert len(state.operandStack) == 1
			
			state.appendInstr(Ret(self))
	
	def __str__(self):
		lines = [str(mir) for mir in self.mir]
		block = '\n    '.join(line for line in lines if line)
		if block:
			block = '    ' + block + '\n'
		
		start = '@{}'.format(self.id)
		if self.ancestors:
			start += ':FROM({})'.format(', '.join('@{}'.format(s.id) for s in self.ancestors))
		else:
			start += ':START'
		if self.inputs:
			start += ':IN({})'.format(', '.join(s.name for s in self.inputs))
		start += ':\n'
		
		end = ''
		if self.outputs:
			end += ':OUT({})'.format(', '.join(s.name for s in self.outputs))
		if self.branchOn:
			end += ':BR({})'.format(self.branchOn.name)
		if self.successors:
			end += ':TO({})'.format(', '.join('@{}'.format(s.id) for s in self.successors))
		else:
			end += ':RET'
		end += '\n'
		
		return start + block + end

class CFGDropPoint(MIR):
	def __init__(self, span):
		super().__init__(span, False)
		self.span = span
		self.mir = []
	
	def append(self, mir):
		self.mir.append(mir)
	
	def writeIR(self, state):
		for mir in self.mir:
			mir.writeIR(state)
	
	def __str__(self):
		if len(self.mir) == 0:
			return '> '
			# return ''
		
		lines = []
		prefix = '> '
		for mir in self.mir:
			lineStr = prefix + str(mir)
			lines.append(lineStr)
			prefix = '| '
		
		return '\n'.join(lines)