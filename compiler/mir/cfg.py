from .mir    import MIR
from .access import SymbolAccess, SymbolRead
from .fncall import FnCall
from .drop   import DropSymbol
from ..ir    import Br, BrIf, Ret, BlockMarker, getInputInfo
from ..log   import logError, logExplain
from ..types import PtrType

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
	
	def cloneForNewBlock(self):
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

class CFGBlock:
	def __init__(self, state, ancestors, span):
		if state:
			self.id = state.blockId
			state.blockId += 1
		else:
			self.id = -1
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
			self.symbolState = { k: v.cloneForNewBlock() for (k, v) in ancestors[0].symbolState.items() }
			for ancestor in ancestors:
				ancestor.successors.append(self)
	
	def cloneForDiscard(self):
		other = CFGBlock(None, None, self.span)
		other.symbolState = { k: v.cloneForNewBlock() for (k, v) in self.symbolState.items() }
		return other
		
	def addReverseAncestor(self, ancestor):
		self.ancestors.append(ancestor)
		ancestor.successors.append(self)
		ancestor.symbolState.update({ k: v.cloneForNewBlock() for (k, v) in self.symbolState.items() })
	
	def addAncestor(self, ancestor):
		self.ancestors.append(ancestor)
		ancestor.successors.append(self)
		self.symbolState.update({ k: v.cloneForNewBlock() for (k, v) in ancestor.symbolState.items() })
	
	def append(self, mir):
		self.mir.append(mir)
	
	def decl(self, symbol):
		self.decls.add(symbol)
		self.symbolState[symbol] = SymbolState(symbol)
	
	def access(self, state, access):
		if not (access.symbol and access.symbol.type):
			return
		
		if access.symbol not in self.symbolState:
			assert not access.symbol.isLocal
			return
		
		info = self.symbolState[access.symbol]
		
		if access.write:
			if access.deref:
				self.derefWriteSymbol(state, access, info)
			else:
				self.writeSymbol(state, access, info)
		elif access.addr:
			self.addrSymbol(state, access, info)
		else:
			self.readSymbol(state, access, info)
		
		info.recordTouch(access)
		if info.wasRedeclared:
			self.decls.add(access.symbol)
		elif not info.wasDeclared:
			self.inputs.add(access.symbol)
	
	def writeSymbol(self, state, access, info):
		if info.init and not info.moved and info.lastUse:
			self.dropLastUse(state, info)
		
		info.init = True
		info.moved = False
	
	def derefWriteSymbol(self, state, access, info):
		self.readSymbol(state, access, info)
		
		symbol = access.symbol
		# isField = access.isFieldAccess
		# field = access.field
		# fieldSpan = access.fieldSpan
		# isIndex = len(access.dynOffsets) > 0
		
		if symbol.type.isPtrType and not symbol.type.mut:
			logError(state, access.lvalueSpan, 'assignment target is not mutable')
		
		# if info.borrows:
		# 	for borrow in info.borrows:
		# 		if borrow.symbol in self.symbolInfo:
		# 			borrowInfo = self.symbolInfo[borrow.symbol]
		# 			borrowInfo.borrowedBy.remove(info.symbol)
		
		# info.borrows = access.rvalue.borrows
		
		# if isField:
		# 	pass
		# else:
		# 	self.dropInd(symbol, access.dropBeforeAssignBlock, access.deref)
		
		# assert access.borrows
		# for borrow in access.borrows:
		# 	borrowInfo = self.loadAndSaveSymbolInfo(borrow.symbol)
		# 	if borrowInfo == None:
		# 		continue
		# 	self.writeSymbol(borrow, borrowInfo)
	
	def readSymbol(self, state, access, info):
		symbol = access.symbol
		
		if not info.init:
			if info.moved:
				logError(state, access.span, 'the value in `{}` has been moved'.format(symbol.name))
				for use in info.lastUses:
					if use.ref:
						logExplain(state, use.span, '`{}` was moved here'.format(symbol.name))
			else:
				logError(state, access.span, '`{}` has not been initialized'.format(symbol.name))
		
		if access.ref:
			info.moved = not access.copy
	
	def addrSymbol(self, state, access, info):
		symbol = access.symbol
		field = access.field
		fieldSpan = access.fieldSpan
		isIndex = len(access.dynOffsets) > 0
		
		if access.type.mut and not access.symbol.mut:
			logError(state, access.span, 'mutable borrow of immutable symbol')
		
		symbol.fixed = True
	
	def dropLastUse(self, state, info):
		if info.lastUse.ref and info.lastUse.copy:
			assert not info.lastUse.symbol.dropFn
			info.lastUse.copy = False
		else:
			self.dropSymbol(state, info.symbol, info.lastUse.dropPoint)
	
	def dropSymbol(self, state, symbol, dropPoint):
		if symbol.type.isOwnedType:
			logError(state, symbol.span, 'owned value was not discarded')
			logExplain(state, dropPoint.span.endSpan(), 'value escapes here')
		
		if symbol.dropFn:
			self.callDropFn(symbol.dropFn, symbol, None, 0, dropPoint)
		
		self.dropFields(state, symbol, None, 0, dropPoint)
		
		dropPoint.append(DropSymbol(symbol, dropPoint.span))
	
	def callDropFn(self, dropFn, symbol, field, fieldBase, dropPoint, indLevel=0):
		span = dropPoint.span
		symbol.fixed = True
		
		# create the fn ref for the fn call
		fnRef = SymbolRead(span)
		fnRef.symbol = dropFn
		fnRef.type = dropFn.type
		fnRef.ref = True
		
		# take the address of the symbol/field
		ptr = SymbolRead(span)
		ptr.symbol = symbol
		
		if indLevel == 0:
			ptr.addr = True
		else:
			ptr.copy = True
			if indLevel > 1:
				ptr.deref = indLevel - 1
		
		if field:
			ptr.isFieldAccess = True if indLevel == 0 else False
			ptr.staticOffset = fieldBase + field.offset
			ptr.type = PtrType(field.type, 1, True)
		else:
			ptr.type = PtrType(symbol.type, 1, True)
		
		# use the fn ref and call the drop fn
		fnCall = FnCall(fnRef, [ptr], [], False, fnRef.type.returnType, span, isDrop=True)
		dropPoint.append(fnCall)
	
	def dropEnum(self, state, symbol, field, fieldBase, dropPoint, indLevel=0):
		symbolType = symbol.type.typeAfterDeref(indLevel) if indLevel > 0 else symbol.type
		t = field.type if field else symbolType
		assert t.isEnumType
		parentDropFn = field.type.dropFn if field else symbol.dropFn
		
		for variant in t.variants:
			if variant.type.isCompositeType:
				for field in variant.type.fields:
					if field.type.isOwnedType and not parentDropFn:
						logError(state, symbol.span, 
							'owned value in field `{}` was not discarded'.format(field.name))
						logExplain(state, span.endSpan(), 'value escapes here')
					elif field.type.dropFn:
						logWarning(state, symbol.span, 
							'I can\'t drop `enum`s properly; field `{}` will not be dropped'.format(field.name))
	
	# def dropField(self, state, symbol, field, block, span):
	# 	fieldInfo = self.symbolInfo[symbol].fieldInfo
	# 	if field not in fieldInfo or not fieldInfo[field].uninit:
	# 		if field.type.isOwnedType:
	# 			logError(state, symbol.span, 
	# 				'owned value in field `{}` was not discarded'.format(field.name))
	# 			logExplain(state, span, 'value escapes here')
			
	# 		if field.type.dropFn:
	# 			self.callDropFn(field.type.dropFn, symbol, field, fieldBase, block.exprs, span)
			
	# 		self.dropFields(state, symbol, field, field.offset, block.exprs, span)
	
	def dropFields(self, state, symbol, field, fieldBase, dropPoint, indLevel=0):
		span = dropPoint.span
		symbolType = symbol.type.typeAfterDeref(indLevel) if indLevel > 0 else symbol.type
		t = field.type if field else symbolType
		parentDropFn = field.type.dropFn if field else symbol.dropFn
		
		if t.isEnumType:
			self.dropEnum(state, symbol, field, fieldBase, dropPoint, indLevel)
			return
		elif not t.isCompositeType:
			return
		
		# fieldInfo = self.symbolInfo[symbol].fieldInfo
		for field in reversed(t.fields):
			# if field not in fieldInfo or not fieldInfo[field].uninit:
				if field.type.isOwnedType and not parentDropFn:
					logError(state, symbol.span, 
						'owned value in field `{}` was not discarded'.format(field.name))
					logExplain(state, span.endSpan(), 'value escapes here')
				
				if field.type.dropFn:
					self.callDropFn(field.type.dropFn, symbol, field, fieldBase, dropPoint, indLevel)
				
				self.dropFields(state, symbol, field, fieldBase + field.offset, dropPoint, indLevel)
	
	def finalize(self, state, successorSymbols=set()):
		assert not self.unreachable
		if self.finalized:
			return
		
		self.finalizeCount += 1
		self.finalized = self.finalizeCount >= len(self.successors)
		finalized = self.finalized
		
		self.outputs.update(successorSymbols)
		self.inputs.update(symbol for symbol in self.outputs if not self.symbolState[symbol].wasTouched)
		for block in self.ancestors:
			block.finalize(state, self.inputs)
		
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
						block.dropSymbol(state, symbol, dropPoint)
					block.mir.insert(0, dropPoint)
			
			for info in self.symbolState.values():
				if info.init and not info.moved and info.lastUse and info.symbol != self.branchOn and not (info.symbol in self.outputs):
					self.dropLastUse(state, info)
				elif self.id == 1 and info.symbol.unused and info.symbol.isParam:
					self.dropSymbol(state, info.symbol, info.symbol.dropPoint)
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
		
		return '\n    '.join(lines)