from .mir    import MIR
from .access import SymbolAccess, SymbolRead
from .fncall import FnCall
from .drop   import DropSymbol
from ..ir    import Br, BrIf, Ret, BlockMarker, Raise, getInputInfo
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
		self.borrows = set()
		self.borrowedBy = {}
		self.staticValue = None
	
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
		other.borrows = set(self.borrows)
		other.borrowedBy = dict(self.borrowedBy)
		return other
	
	def merge(self, other):
		self.unused = self.unused or other.unused
		self.init = self.init and other.init
		self.moved = self.moved or other.moved
		self.borrows.update(other.borrows)
		self.borrowedBy.update(other.borrowedBy)
	
	def recordTouch(self, access):
		if self.wasTouched == False and access.write and not (access.deref or access.isFieldAccess):
			self.wasRedeclared = True
		
		self.unused = False
		self.wasTouched = True
		self.wasRead = access.ref
		self.wasWritten = access.write
		self.lastUse = access

class CFGBlock:
	def __init__(self, ancestors, span):
		self.id = None
		self.span = span
		self.inputs = set()
		self.outputs = set()
		self.mir = []
		self.symbolState = {}
		self.branchOn = None
		self.ancestors = []
		self.successors = []
		self.irBlockDef = None
		self.irInputSymbols = None
		self.didReturn = False
		self.didBreak = False
		self.unreachable = False
		self.hasReverseSuccessor = False
		self.reverseAncestorCount = 0
		self.finalized = False
		self.finalizedInputs = False
		self.finalizedCount = 0
		self.isEmpty = True
		
		if ancestors:
			self.ancestors = ancestors
			self.initStateFromAncestors(ancestors)
			for ancestor in ancestors:
				ancestor.successors.append(self)
	
	def cloneForDiscard(self):
		other = CFGBlock(None, self.span)
		other.symbolState = { k: v.cloneForNewBlock() for (k, v) in self.symbolState.items() }
		return other
	
	def initStateFromAncestors(self, ancestors):
		assert not self.symbolState
		symbolStateDicts = [a.symbolState for a in ancestors]
		symbols = set()
		for s in symbolStateDicts:
			symbols.update(s.keys())
		for symbol in symbols:
			for s in symbolStateDicts:
				if symbol not in s:
					continue
				elif symbol in self.symbolState:
					self.symbolState[symbol].merge(s[symbol])
				else:
					self.symbolState[symbol] = s[symbol].cloneForNewBlock()
	
	def addReverseAncestor(self, state, ancestor):
		self.reverseAncestorCount += 1
		ancestor.hasReverseSuccessor = True
		self.ancestors.append(ancestor)
		ancestor.successors.append(self)
		
		firstAncestor = self.ancestors[0]
		inputState = firstAncestor.symbolState
		for inputInfo in inputState.values():
			reverseInfo = ancestor.symbolState[inputInfo.symbol]
			
			inputIsLive = inputInfo.init and not inputInfo.moved
			reverseIsLive = reverseInfo.init and not reverseInfo.moved
			if inputIsLive and not reverseIsLive:
				logError(state, reverseInfo.lastUse.span, 
					'value was moved out; `{}` must be reinitialized before next loop iteration'.format(reverseInfo.symbol.name))
			# elif reverseIsLive and not inputIsLive:
			# 	logError(self.state, reverseInfo.lastUse.span, 
			# 		'value was moved out; `{}` must be reinitialized before next loop iteration'.format(reverseInfo.symbol.name))
	
	def addAncestor(self, state, ancestor):
		assert not self.mir
		self.ancestors.append(ancestor)
		ancestor.successors.append(self)
		
		s = ancestor.symbolState
		for info in ancestor.symbolState.values():
			symbol = info.symbol
			if symbol in self.symbolState:
				self.symbolState[symbol].merge(s[symbol])
			else:
				self.symbolState[symbol] = s[symbol].cloneForNewBlock()
	
	def append(self, mir, isEmpty=False):
		self.mir.append(mir)
		self.isEmpty = self.isEmpty and isEmpty
	
	def decl(self, symbol):
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
			elif access.isFieldAccess:
				self.fieldWriteSymbol(state, access, info)
			else:
				self.writeSymbol(state, access, info)
		elif access.addr:
			self.addrSymbol(state, access, info)
		elif access.deref:
			self.derefSymbol(state, access, info)
		else:
			self.readSymbol(state, access, info)
		
		info.recordTouch(access)
		if not (info.wasDeclared or info.wasRedeclared):
			self.inputs.add(access.symbol)
	
	def writeSymbol(self, state, access, info):
		if info.init and not info.moved and info.lastUse:
			self.dropLastUse(state, info)
		
		if not access.symbol.mut and info.init:
			logError(state, access.lvalueSpan, 'assignment target is not mutable')
		
		info.borrows = access.rvalue.borrows
		for borrowed in info.borrows:
			borrowedInfo = self.symbolState[borrowed.symbol]
			borrowedInfo.borrowedBy[info.symbol] = access.rvalue
		
		info.init = True
		info.moved = False
	
	def fieldWriteSymbol(self, state, access, info):
		pass
	
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
	
	def derefSymbol(self, state, access, info):
		self.readSymbol(state, access, info)
		
		symbol = access.symbol
		
		if not info.init:
			if info.moved:
				logError(state, access.span, 'the value in `{}` has been moved'.format(symbol.name))
				for use in info.lastUses:
					if use.ref:
						logExplain(state, use.span, '`{}` was moved here'.format(symbol.name))
			else:
				logError(state, access.span, '`{}` has not been initialized'.format(symbol.name))
		
		for borrow in info.borrows:
			errCount = 0
			if borrow.symbol not in state.scope.symbols:
				if errCount == 0:
					logError(state, access.span, 'borrowed value has gone out of scope')
				errCount += 1
				countStr = '' if errCount < 2 else '({}) '.format(errCount)
				logExplain(state, borrow.span, 'borrow {}originally occurred here'.format(countStr))
			# elif info.symbol != borrow.symbol:
			# 	# borrowInfo = self.symbolInfo[borrow.symbol]
			# 	borrowInfo.borrowedBy.add(info.symbol)
			# 	self.setLastUse(borrowInfo, use, isRead)
		
		if access.ref:
			info.moved = not access.copy
	
	def readSymbol(self, state, access, info):
		symbol = access.symbol
		
		if info.moved or not info.init:
			if info.moved:
				logError(state, access.span, 'the value in `{}` has been moved'.format(symbol.name))
				# for use in info.lastUses:
					# if use.ref:
				logExplain(state, info.lastUse.span, '`{}` was moved here'.format(symbol.name))
			else:
				logError(state, access.span, '`{}` has not been initialized'.format(symbol.name))
		
		access.borrows = info.borrows
		
		for borrow in info.borrows:
			borrowedInfo = self.symbolState[borrow.symbol]
			borrowedInfo.recordTouch(access)
		
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
	
	def releaseBorrows(self, symbol):
		for borrow in self.symbolState[symbol].borrows:
			del self.symbolState[borrow.symbol].borrowedBy[symbol]
	
	def dropLastUse(self, state, info):
		if info.lastUse.ref and \
			info.lastUse.copy and \
			not info.symbol.dropFn and \
			not info.lastUse.borrow and \
			not info.borrowedBy:
			self.releaseBorrows(info.symbol)
			info.lastUse.copy = False
		else:
			self.dropSymbol(state, info.symbol, info.lastUse.dropPoint)
	
	def dropSymbol(self, state, symbol, dropPoint):
		self.releaseBorrows(symbol)
		
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
	
	# def dropInd(self, symbol, block, indLevel):
	# 	exprs = []
		
	# 	# if symbol.type.isOwnedType:
	# 	# 	logError(self.state, symbol.span, 'owned value was not discarded')
	# 	# 	logExplain(self.state, block.span.endSpan(), 'value escapes here')
		
	# 	if symbol.type.baseType.dropFn:
	# 		self.callDropFn(symbol.type.baseType.dropFn, symbol, None, 0, exprs, block.span, indLevel=indLevel)
		
	# 	self.dropFields(symbol, None, 0, exprs, block.span, indLevel=indLevel)
		
	# 	block.exprs[:0] = exprs
	
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
	
	def finalizeInputs(self, state, indent=0):
		# print('  ' * indent, self.id)
		if self.finalizedInputs:
			return
		
		self.finalizedCount += 1
		self.finalizedInputs = self.finalizedCount > self.reverseAncestorCount + 1
		
		for block in self.successors:
			# if not self.hasReverseSuccessor:
			# 	block.reverseAncestorCount += self.reverseAncestorCount
			block.finalizeInputs(state, indent+1)
			self.outputs.update(block.inputs)
		
		borrowedOutputs = set()
		for symbol in self.outputs:
			info = self.symbolState[symbol]
			borrowedOutputs.update({ b.symbol for b in info.borrows })
		self.outputs.update(borrowedOutputs)
		
		self.inputs.update(symbol for symbol in self.outputs if not self.symbolState[symbol].wasTouched)
	
	def finalize(self, state):
		if self.finalized:
			return
		
		self.finalized = True
		
		for block in self.ancestors:
			self.inputs.update(block.outputs)
		
		dropInBlock = self.inputs - self.outputs - {self.branchOn}
		dropInBlock = set(symbol for symbol in dropInBlock if not self.symbolState[symbol].wasTouched)
		if dropInBlock:
			dropPoint = CFGDropPoint(block.span)
			for symbol in dropInBlock:
				self.dropSymbol(state, symbol, dropPoint)
			self.mir.insert(0, dropPoint)
		
		for info in self.symbolState.values():
			if info.init and not info.moved and info.lastUse and \
				info.symbol != self.branchOn and not (info.symbol in self.outputs):
				self.dropLastUse(state, info)
			elif self.id == 1 and info.symbol.unused and info.symbol.isParam:
				self.dropSymbol(state, info.symbol, info.symbol.dropPoint)
	
	def writeIR(self, state):
		if self.ancestors:
			assert self.irBlockDef
			state.setupLocals(self.irBlockDef.inputs, self.irInputSymbols)
			state.appendInstr(BlockMarker(self, self.irBlockDef.index, self.id))
		
		for mir in self.mir:
			mir.writeIR(state)
		
		inputTypes, inputSymbols = getInputInfo(state)
		if self.successors:
			if self.branchOn:
				inputTypes.pop()
				inputSymbols.pop()
			
			for block in self.successors:
				if block.irBlockDef == None:
					block.irBlockDef = state.defBlock(inputTypes, block.id)
					block.irInputSymbols = inputSymbols
				else:
					assert len(state.operandStack) == len(block.irInputSymbols)
					fixStack = False
					for (found, expected) in zip(state.operandStack, block.irInputSymbols):
						if found.symbol != expected:
							fixStack = True
							break
					if fixStack:
						for symbol in block.irInputSymbols:
							stackOffset = state.localOffset(symbol)
							state.appendInstr(Raise(self, stackOffset))
			
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