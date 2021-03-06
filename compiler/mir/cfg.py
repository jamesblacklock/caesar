from .mir    import MIR, TypeModifiers
from .access import SymbolAccess, SymbolRead, AccessType
from .fncall import FnCall
from .drop   import DropSymbol
from ..ir    import Br, BrIf, Dup, Ret, BlockMarker, Raise, getInputInfo
from ..log   import logError, logExplain
from ..types import PtrType

class FieldState:
	def __init__(self, field, init):
		self.field = field
		self.init = init
		self.moved = False
		self.movedSpan = None
		# self.borrows = set()
	
	def cloneForNewBlock(self):
		other = FieldState(self.field, self.init)
		other.moved = self.moved
		other.movedSpan = self.movedSpan
		# other.borrows = set(self.borrows)
		return other

class FieldStates(dict):
	def __getitem__(self, field):
		if field in self:
			return super().__getitem__(field)
		else:
			info = FieldState(field, True)
			self[field] = info
			return info
	
	def cloneForNewBlock(self):
		other = FieldStates()
		for (k, v) in self.items():
			other[k] = v.cloneForNewBlock()
		return other

class SymbolState:
	def __init__(self, symbol):
		self.symbol = symbol
		self.wasDeclared = True
		self.wasRedeclared = False
		self.wasTouched = True
		self.wasRead = False
		self.wasWritten = False
		self.lastUse = None
		self.movedSpan = None
		self.unused = True
		self.init = symbol.isParam
		self.moved = False
		self.borrows = set()
		self.borrowedBy = {}
		self.staticValue = None
		self.fieldState = FieldStates()
		self.typeModifiers = TypeModifiers()
	
	def cloneForNewBlock(self):
		other = SymbolState(self.symbol)
		other.wasDeclared = False
		other.wasRedeclared = False
		other.wasTouched = False
		other.wasRead = False
		other.wasWritten = False
		other.lastUse = None
		other.movedSpan = self.movedSpan
		other.unused = self.unused
		other.init = self.init
		other.moved = self.moved
		other.borrows = set(self.borrows)
		other.borrowedBy = dict(self.borrowedBy)
		other.fieldState = self.fieldState.cloneForNewBlock()
		return other
	
	def merge(self, other):
		self.unused = self.unused or other.unused
		self.init = self.init and other.init
		self.moved = self.moved or other.moved
		self.movedSpan = other.movedSpan
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
		self.returnAccess = None
		
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
				if reverseInfo.symbol.mut:
					message = 'value was moved out; `{}` must be reinitialized before next loop iteration'
				else:
					message = 'value was moved out; `{}` must be initialized during next loop iteration'
				logError(state, reverseInfo.movedSpan, message.format(reverseInfo.symbol.name))
	
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
		
		if access.accessType == AccessType.REF_READ:
			self.refRead(state, access, info)
		elif access.accessType == AccessType.REF_WRITE:
			self.refWrite(state, access, info)
		elif access.accessType == AccessType.REF_ADDR:
			self.refAddr(state, access, info)
		elif access.accessType == AccessType.FIELD_READ:
			self.fieldRead(state, access, info)
		elif access.accessType == AccessType.FIELD_WRITE:
			self.fieldWrite(state, access, info)
		elif access.accessType == AccessType.FIELD_ADDR:
			self.fieldAddr(state, access, info)
		elif access.accessType == AccessType.DEREF_READ:
			self.derefRead(state, access, info)
		elif access.accessType == AccessType.DEREF_WRITE:
			self.derefWrite(state, access, info)
		elif access.accessType == AccessType.DEREF_FIELD_READ:
			self.derefFieldRead(state, access, info)
		elif access.accessType == AccessType.DEREF_FIELD_WRITE:
			self.derefFieldWrite(state, access, info)
		else:
			assert 0
		
		info.recordTouch(access)
		for borrowed in info.borrows:
			borrowedInfo = self.symbolState[borrowed.symbol]
			borrowedInfo.recordTouch(access)
		
		if info.symbol.isLocal and not (info.wasDeclared or info.wasRedeclared):
			self.inputs.add(access.symbol)
	
	def refLiveCheck(self, state, access, info):
		if not info.init:
			logError(state, access.span, '`{}` has not been initialized'.format(access.symbol.name))
		elif info.moved:
			logError(state, access.span, 'the value in `{}` has been moved'.format(access.symbol.name))
			logExplain(state, info.movedSpan, '`{}` was moved here'.format(access.symbol.name))
	
	def refRead(self, state, access, info):
		symbol = info.symbol
		
		self.refLiveCheck(state, access, info)
		
		access.borrows = info.borrows
		access.typeModifiers = info.typeModifiers.clone()
		
		info.moved = not access.copy
		info.movedSpan = access.span
	
	def refWrite(self, state, access, info):
		if info.init and not info.moved and info.lastUse:
			self.dropLastUse(state, info)
		
		if not info.symbol.mut and info.init:
			logError(state, access.lvalueSpan, 'assignment target is not mutable')
		
		info.borrows = access.rvalue.borrows
		for borrowed in info.borrows:
			borrowedInfo = self.symbolState[borrowed.symbol]
			borrowedInfo.borrowedBy[info.symbol] = access.rvalue
		
		if access.rvalue.typeModifiers:
			info.typeModifiers = access.rvalue.typeModifiers.clone()
		
		info.init = True
		info.moved = False
	
	def refAddr(self, state, access, info):
		if access.type.mut and not info.symbol.mut:
			logError(state, access.span, 'mutable borrow of immutable symbol')
		
		info.symbol.fixed = True
	
	def fieldLiveCheck(self, state, access, fieldInfo):
		if not fieldInfo.init:
			logError(state, access.fieldSpan, 'field `{}` has not been initialized'.format(access.field.name))
		elif fieldInfo.moved:
			logError(state, access.fieldSpan, 'the value in field `{}` has been moved'.format(access.field.name))
			logExplain(state, fieldInfo.movedSpan, 'field `{}` was moved here'.format(access.field.name))
	
	def fieldRead(self, state, access, info):
		symbol = info.symbol
		field = access.field
		fieldSpan = access.fieldSpan
		# isIndex = len(access.dynOffsets) > 0
		
		self.refLiveCheck(state, access, info)
		
		if field:
			# if field not in info.fieldInfo:
			# 	uninit = info.uninit or info.typeModifiers and field in info.typeModifiers.uninitFields
			# 	fieldInfo = FieldInfo(field, uninit)
			# 	info.fieldInfo[field] = fieldInfo
			# else:
			fieldInfo = info.fieldState[field]
			self.fieldLiveCheck(state, access, fieldInfo)
			fieldInfo.movedSpan = fieldSpan
			fieldInfo.moved = not field.type.isCopyable
			
			# movedFields = allFields(use.field.type)
			# movedFields.add(use.field)
			# info.typeModifiers.uninitFields.update(movedFields)
	
	def fieldWrite(self, state, access, info):
		symbol = info.symbol
		field = access.field
		fieldSpan = access.fieldSpan
		# isIndex = len(access.dynOffsets) > 0
		
		self.refLiveCheck(state, access, info)
		
		if field:
			fieldInfo = info.fieldState[field]
			if fieldInfo.init and not fieldInfo.moved:
				self.dropField(state, symbol, field, access.beforeWriteDropPoint)
			
			# initFields = allFields(use.field.type)
			# initFields.difference_update(use.rvalue.typeModifiers.uninitFields)
			# info.typeModifiers.uninitFields.difference_update(initFields)
			# info.typeModifiers.uninitFields.discard(use.field)
				
			fieldInfo.moved = False
			fieldInfo.init = True
	
	def fieldAddr(self, state, access, info):
		pass
	
	def derefRead(self, state, access, info):
		self.refRead(state, access, info)
		
		symbol = access.symbol
		
		if not info.init:
			if info.moved:
				logError(state, access.span, 'the value in `{}` has been moved'.format(symbol.name))
				for use in info.lastUses:
					if use.ref:
						logExplain(state, use.span, '`{}` was moved here'.format(symbol.name))
			else:
				logError(state, access.span, '`{}` has not been initialized'.format(symbol.name))
		
		for borrowed in info.borrows:
			errCount = 0
			if borrowed.symbol not in state.scope.symbols:
				if errCount == 0:
					logError(state, access.span, 'borrowed value has gone out of scope')
				errCount += 1
				countStr = '' if errCount < 2 else '({}) '.format(errCount)
				logExplain(state, borrowed.span, 'borrow {}originally occurred here'.format(countStr))
			
			borrowInfo = self.symbolState[borrowed.symbol]
			if not borrowed.type.isCopyable:
				self.refRead(state, borrowed, borrowInfo)
				self.dropSymbol(state, borrowed.symbol, access.dropPoint)
	
	def derefWrite(self, state, access, info):
		self.refRead(state, access, info)
		
		symbol = access.symbol
		
		if symbol.type.isPtrType and not symbol.type.mut:
			logError(state, access.lvalueSpan, 'assignment target is not mutable')
		
		# if info.borrows:
		# 	for borrowed in info.borrows:
		# 		if borrowed.symbol in self.symbolState:
		# 			borrowInfo = self.symbolState[borrowed.symbol]
		# 			borrowInfo.borrowedBy.remove(info.symbol)
		
		# info.borrows = access.rvalue.borrows
		
		self.dropInd(state, symbol, access.beforeWriteDropPoint, access.deref)
		
		errCount = 0
		for borrowed in info.borrows:
			if borrowed.symbol not in state.scope.symbols:
				if errCount == 0:
					logError(state, access.span, 'borrowed value has gone out of scope')
				errCount += 1
				countStr = '' if errCount < 2 else '({}) '.format(errCount)
				logExplain(state, borrowed.span, 'borrow {}originally occurred here'.format(countStr))
			
			borrowInfo = self.symbolState[borrowed.symbol]
			borrowInfo.borrows = access.rvalue.borrows
			for borrowedBorrowed in borrowInfo.borrows:
				borrowedBorrowedInfo = self.symbolState[borrowedBorrowed.symbol]
				borrowedBorrowedInfo.borrowedBy[info.symbol] = access.rvalue
			
			borrowInfo.init = True
			borrowInfo.moved = False
	
	def derefFieldRead(self, state, access, info):
		pass
	
	def derefFieldWrite(self, state, access, info):
		pass
	
	def releaseBorrows(self, symbol):
		for borrowed in self.symbolState[symbol].borrows:
			del self.symbolState[borrowed.symbol].borrowedBy[symbol]
	
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
	
	def dropField(self, state, symbol, field, dropPoint):
		fieldInfo = self.symbolState[symbol].fieldState[field]
		if fieldInfo.init and not fieldInfo.moved:
			if field.type.isOwnedType:
				logError(state, symbol.span, 'owned value in field `{}` was not discarded'.format(field.name))
				logExplain(state, dropPoint.span, 'value escapes here')
			
			if field.type.dropFn:
				self.callDropFn(field.type.dropFn, symbol, field, fieldBase, dropPoint)
			
			self.dropFields(state, symbol, field, field.offset, dropPoint)
	
	def dropInd(self, state, symbol, dropPoint, indLevel):
		if symbol.type.typeAfterDeref(indLevel).isOwnedType:
			logError(state, symbol.span, 'owned value was not discarded')
			logExplain(state, dropPoint.span.endSpan(), 'value escapes here')
		
		if symbol.type.baseType.dropFn:
			self.callDropFn(symbol.type.baseType.dropFn, symbol, None, 0, dropPoint, indLevel=indLevel)
		
		self.dropFields(state, symbol, None, 0, dropPoint, indLevel=indLevel)
	
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
		
		# fieldState = self.symbolState[symbol].fieldState
		for field in reversed(t.fields):
			# if field not in fieldState or not fieldState[field].uninit:
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
		
		if not self.successors:
			for param in state.fn.params:
				info = self.symbolState[param]
				if info.borrows:
					scopeErrCount = 0
					for borrowed in info.borrows:
						if scopeErrCount == 0:
							logError(state, param.span, 
								'borrowed value in `{}` escapes the function in which it was defined'.format(param.name))
						scopeErrCount += 1
						countStr = '' if scopeErrCount < 2 else '({}) '.format(scopeErrCount)
						logExplain(state, borrowed.span, 'borrow {}originally occurred here'.format(countStr))
			
			if self.returnAccess:
				numOutputs = 1
				info = self.symbolState[self.returnAccess.symbol]
				if info.borrows:
					scopeErrCount = 0
					for borrowed in info.borrows:
						if borrowed.symbol != self.returnAccess.symbol:
							numOutputs += 1
						if not borrowed.symbol.isParam:
							if scopeErrCount == 0:
								logError(state, self.returnAccess.span, 
									'borrowed value in return escapes the function in which it was defined')
							scopeErrCount += 1
							countStr = '' if scopeErrCount < 2 else '({}) '.format(scopeErrCount)
							logExplain(state, borrowed.span, 'borrow {}originally occurred here'.format(countStr))
					
					assert state.failed and numOutputs == len(self.outputs) or numOutputs == 1 and len(self.outputs) == 1
	
	def writeIR(self, state, noReturn=False):
		if self.ancestors:
			assert self.irBlockDef
			state.setupLocals(self.irBlockDef.inputs, self.irInputSymbols)
			state.appendInstr(BlockMarker(self, self.irBlockDef.index, self.id))
		
		for mir in self.mir:
			mir.writeIR(state)
		
		inputTypes, inputSymbols = getInputInfo(state)
		if self.successors:
			if self.branchOn:
				if self.branchOn in self.outputs:
					state.appendInstr(Dup(self, 0))
				else:
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
		elif not noReturn:
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
	
	def staticSideEffects(self, state):
		return True
	
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