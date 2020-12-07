class Scope:
	def __init__(self, state, parent, scopeType, item):
		self.state = state
		self.parent = parent
		self.type = scopeType
		self.contracts = {}
		self.symbolTable = {}
		self.symbolInfo = {}
		self.name = None
		self.didBreak = False
		self.didReturn = False
		self.fnDecl = None
		self.mod = None
		self.loopDepth = 0
		self.ifExpr = None
		self.loopExpr = None
		self.ifBranchOuterSymbolInfo = None
		self.dropBlock = None
		self.allowUnsafe = False
		self.acquireDefault = None
		self.releaseDefault = None
		self.acquireDefaultSet = False
		self.releaseDefaultSet = False
		self._span = None
		
		if parent:
			self.contracts.update(parent.contracts)
			self.loopDepth = parent.loopDepth
			self.acquireDefault = parent.acquireDefault
			self.releaseDefault = parent.releaseDefault
			if not self.fnDecl:
				self.fnDecl = parent.fnDecl
			if not self.mod:
				self.mod = parent.mod
			if parent.allowUnsafe:
				self.allowUnsafe = True
			if not self.loopExpr:
				self.loopExpr = parent.loopExpr
		
		if self.loopDepth > 0 or scopeType == ScopeType.LOOP:
			self.loopDepth += 1
		
		if self.type == ScopeType.MOD:
			self.setMod(item)
		elif self.type == ScopeType.FN:
			self.setFn(item)
		elif self.type == ScopeType.LOOP:
			self.setLoop(item)
		elif self.type == ScopeType.BLOCK:
			self.setBlock(item)
	
	@property
	def span(self):
		if self._span == None and self.ifExpr:
			self._span = self.ifExpr.block.span if self.type == ScopeType.IF else self.ifExpr.elseBlock.span
		return self._span
	
	def setMod(self, mod):
		self.symbolTable = mod.symbolTable
		self.name = mod.name
		self.mod = mod
		self._span = mod.span
		if mod.acquireDefault:
			self.acquireDefault = mod.acquireDefault
		if mod.releaseDefault:
			self.releaseDefault = mod.releaseDefault
	
	def setFn(self, fn):
		self._span = fn.span
		self.fnDecl = fn
		self.allowUnsafe = fn.unsafe
	
	def setBlock(self, block):
		self._span = block.span
		self.allowUnsafe = self.allowUnsafe or (True if block and block.unsafe else False)
	
	def setLoop(self, loop):
		self._span = loop.span
		self.loopExpr = loop
	
	def intersectContracts(self, contracts):
		if contracts == None:
			return
		
		for contract in contracts.values():
			if contract.symbol in self.contracts:
				self.contracts[contract.symbol] = self.contracts[contract.symbol].intersect(contract)
			else:
				self.contracts[contract.symbol] = contract
	
	def setSymbolTable(self, symbolTable):
		self.symbolTable = symbolTable
	
	def finalize(self):
		if self.type == ScopeType.MOD:
			return None
		
		outerSymbolInfo = {}
		for info in self.symbolInfo.values():
			if info.wasDeclared:
				if info.symbol.unused:
					if info.symbol.isParam:
						self.dropSymbol(info.symbol, info.symbol.dropBlock)
					else:
						logWarning(self.state, info.symbol.span, 'unused symbol: `{}`'.format(info.symbol.name))
				else:
					for block in info.dropInBlock:
						self.dropSymbol(info.symbol, block)
					for (lastUse, loopExpr) in info.lastUses.items():
						# if info.symbol.isParam and lastUse.borrows:
						# 	for borrow in lastUse.borrows:
						# 		if borrow.symbol != info.symbol:
						# 			logError(self.state, lastUse.span, 'error')
						
						if lastUse.ref:
							if loopExpr and loopExpr != self.loopExpr and info.symbol.type.isCopyable:
							# if loopExpr and info.symbol.type.isCopyable:
								lastUse.copy = True
								for (br, otherLoopExpr) in info.breaksAfterMove.items():
									if loopExpr == otherLoopExpr:
										self.dropSymbol(info.symbol, br.dropBlock)
							elif lastUse.borrows:
								if lastUse.copy:
									self.dropSymbol(info.symbol, lastUse.dropBlock)
								# elif loopExpr and loopExpr != self.loopExpr and info.symbol.type.isCopyable:
								# 	lastUse.copy = True
								# 	for borrow in lastUse.borrows:
								# 		if borrow.symbol == info.symbol:
								# 			for (br, otherLoopExpr) in info.breaksSinceLastUse.items():
								# 				if loopExpr == otherLoopExpr:
								# 					self.dropSymbol(info.symbol, br.dropBlock)
								else:
									for borrow in lastUse.borrows:
										if borrow.symbol == info.symbol:
											self.dropSymbol(info.symbol, lastUse.dropBlock)
											break
							elif info.symbol.dropFn and info.symbol.type.isCopyable:
								lastUse.copy = True
								self.dropSymbol(info.symbol, lastUse.dropBlock)
						else:
							if loopExpr and loopExpr != self.loopExpr:
								for (br, otherLoopExpr) in info.breaksSinceLastUse.items():
									if loopExpr == otherLoopExpr:
										self.dropSymbol(info.symbol, br.dropBlock)
							else:
								self.dropSymbol(info.symbol, lastUse.dropBlock)
			elif not self.didReturn or info.didDropInBlock:
				outerSymbolInfo[info.symbol] = info
				
				if info.moved and self.type == ScopeType.LOOP:
					parentInfo = self.parent.loadSymbolInfo(info.symbol)
					if not (parentInfo.moved or info.symbol.type.isCopyable):
						logError(self.state, list(info.lastUses)[0].span, 
							'value was moved out; `{}` must be reinitialized before next loop iteration'.format(info.symbol.name))
		
		if self.type in (ScopeType.IF, ScopeType.ELSE):
			for info in self.symbolInfo.values():
				if info.didDropInBlock:
					info.dropInBlock.add(self.ifExpr.block if ScopeType.IF else self.ifExpr.elseBlock)
		
		if self.type == ScopeType.IF and self.didReturn:
			outerSymbolInfo = None
		elif self.type == ScopeType.ELSE:
			if self.didReturn:
				outerSymbolInfo = self.ifBranchOuterSymbolInfo
			elif self.ifBranchOuterSymbolInfo:
				ifInfo = self.ifBranchOuterSymbolInfo
				elseInfo = outerSymbolInfo
				symbols = set(ifInfo.keys())
				symbols.update(elseInfo.keys())
				for symbol in symbols:
					if symbol in ifInfo and symbol in elseInfo:
						info1 = ifInfo[symbol]
						info2 = elseInfo[symbol]
						parentInfo = self.parent.loadSymbolInfo(info1.symbol)
						
						info1.lastUses = {k: v for (k, v) in info1.lastUses.items() if k not in parentInfo.lastUses}
						if info1.lastUses:
							info2.lastUses = {k: v for (k, v) in info2.lastUses.items() if k not in parentInfo.lastUses}
						info1.lastUses.update(info2.lastUses)
						
						
						info1.returnsAfterMove.update(info2.returnsAfterMove)
						info1.returnsSinceLastUse.update(info2.returnsSinceLastUse)
						
						info1.breaksAfterMove = {k: v for (k, v) in info1.breaksAfterMove.items() if k not in parentInfo.breaksAfterMove}
						info1.breaksAfterMove.update(info2.breaksAfterMove)
						
						info1.breaksSinceLastUse = {k: v for (k, v) in info1.breaksSinceLastUse.items() if k not in parentInfo.breaksSinceLastUse}
						info1.breaksSinceLastUse.update(info2.breaksSinceLastUse)
						
						info1.dropInBlock.update(info2.dropInBlock)
						info1.borrows.update(info2.borrows)
						# info.fieldInfo = { k: v.clone() for (k, v) in self.fieldInfo.items() }
						info1.borrowedBy.update(info2.borrowedBy)
						info1.usesOfBorrowsSinceLastUse.update(info2.usesOfBorrowsSinceLastUse)
						
						if info2.maybeMoved or info1.moved != info2.moved:
							info1.moved = True
							info1.maybeMoved = True
						if info2.maybeUninit or info1.uninit != info2.uninit:
							info1.typeModifiers.uninit = True
							info1.maybeUninit = True
						outerSymbolInfo[symbol] = info1
					else:
						if symbol in ifInfo:
							info = ifInfo[symbol]
							block = self.ifExpr.elseBlock
							outerSymbolInfo[symbol] = info
						else:
							info = elseInfo[symbol]
							block = self.ifExpr.block
						
						if not self.loopExpr:
							info.dropInBlock.add(block)
							info.didDropInBlock = True
						
						parentInfo = self.parent.loadSymbolInfo(symbol)
						if parentInfo.moved != info.moved:
							info.moved = True
							info.maybeMoved = True
						if parentInfo.uninit != info.uninit:
							info.typeModifiers.uninit = True
							info.maybeUninit = True
		
		return outerSymbolInfo
	
	def doReturn(self, access, ret):
		symbol = access.symbol if access else None
		
		if symbol:
			info = self.loadSymbolInfo(symbol)
			if info.borrows:
				scopeErrCount = 0
				for borrow in info.borrows:
					if not borrow.symbol.isParam:
						if scopeErrCount == 0:
							logError(self.state, access.span, 
								'borrowed value in return may escape the function in which it was defined')
						scopeErrCount += 1
						countStr = '' if scopeErrCount < 2 else '({}) '.format(scopeErrCount)
						logExplain(self.state, borrow.span, 'borrow {}originally occurred here'.format(countStr))
		
		for param in self.fnDecl.type.params:
			info = self.loadSymbolInfo(param)
			if info.borrows:
				scopeErrCount = 0
				for borrow in info.borrows:
					if scopeErrCount == 0:
						logError(self.state, param.span, 
							'borrowed value in `{}` may escape the function in which it was defined'.format(param.name))
					scopeErrCount += 1
					countStr = '' if scopeErrCount < 2 else '({}) '.format(scopeErrCount)
					logExplain(self.state, borrow.span, 'borrow {}originally occurred here'.format(countStr))
		
		self.didReturn = True
		if self.type == ScopeType.FN:
			return
		
		scope = self.parent
		while True:
			for info in scope.symbolInfo.values():
				if not info.wasDeclared or info.symbol == symbol:
					continue
				
				info = self.loadSymbolInfo(info.symbol)
				if info.moved and info.symbol.type.isCopyable:
					info.returnsAfterMove.add(ret)
				elif not info.uninit:
					info.returnsSinceLastUse.add(ret)
			
			if scope.type == ScopeType.FN:
				break
			
			scope = scope.parent
	
	def doBreak(self, expr, isContinue):
		stopAt = ScopeType.LOOP if isContinue else ScopeType.FN
		
		self.didBreak = True
		if self.type == stopAt:
			return
		
		symbols = set()
		scope = self.parent
		while True:
			for info in scope.symbolInfo.values():
				if info.wasDeclared:
					symbols.add(info.symbol)
			
			if scope.type == stopAt:
				break
			
			scope = scope.parent
		
		for symbol in symbols:
			info = self.loadAndSaveSymbolInfo(symbol)
			lastUses = dict(info.lastUses)
			if self.ifBranchOuterSymbolInfo and info.symbol in self.ifBranchOuterSymbolInfo:
				lastUses.update(self.ifBranchOuterSymbolInfo[info.symbol].lastUses)
			
			for (lastUse, loopExpr) in lastUses.items():
				# if loopExpr == self.loopExpr:
				if loopExpr == self.loopExpr and lastUse.symbol == info.symbol:
					if info.moved and info.symbol.type.isCopyable:
						info.breaksAfterMove[expr] = loopExpr
					elif not info.uninit:
						info.breaksSinceLastUse[expr] = loopExpr
					break
		
		# scope = self.parent
		# while True:
		# 	for info in scope.symbolInfo.values():
		# 		# 
		# 		# 
		# 		if not info.wasDeclared:
		# 			continue
				
		# 		# if self.ifBranchOuterSymbolInfo and info.symbol in self.ifBranchOuterSymbolInfo:
		# 		# 	info = self.ifBranchOuterSymbolInfo[info.symbol]
		# 		# else:
		# 		info = self.loadAndSaveSymbolInfo(info.symbol)
				
		# 		for (lastUse, loopExpr) in info.lastUses.items():
		# 			# if loopExpr == self.loopExpr:
		# 			if loopExpr == self.loopExpr and lastUse.symbol == info.symbol:
		# 				if info.moved and info.symbol.type.isCopyable:
		# 					info.breaksAfterMove[expr] = loopExpr
		# 				elif not info.uninit:
		# 					info.breaksSinceLastUse[expr] = loopExpr
		# 				break
			
		# 	if scope.type == stopAt:
		# 		break
			
		# 	scope = scope.parent
	
	def setLastUse(self, info, use, isRead):
		if use.write and (use.isFieldAccess or not use.deref and (not info.uninit or info.maybeUninit)):
			if use.isFieldAccess:
				if use.field:
					self.dropField(use.symbol, use.field, use.dropBeforeAssignBlock, use.span)
			else:
				for (lastUse, loopExpr) in info.lastUses.items():
					if lastUse.write and not lastUse.isFieldAccess and not lastUse.deref:
						self.dropSymbol(info.symbol, lastUse.dropBlock)
		elif info.moved and info.symbol.type.isCopyable and \
			(isRead or info.symbol.dropFn):
			for lastUse in info.lastUses:
				if lastUse.ref:
					lastUse.copy = True
			
			if info.returnsAfterMove:
				for ret in info.returnsAfterMove:
					self.dropSymbol(info.symbol, ret.dropBlock)
				info.returnsAfterMove = set()
				
			if info.breaksAfterMove:
				for (br, loopExpr) in info.breaksAfterMove.items():
					if self.loopExpr == loopExpr:
						self.dropSymbol(info.symbol, br.dropBlock)
				info.breaksAfterMove = {}
			
			info.moved = False
			info.maybeMoved = False
			info.typeModifiers.uninit = False
			info.maybeUninit = False
		
		if isRead:
			if not use.write and not use.addr and use.field and not use.field.type.isCopyable:
				movedFields = allFields(use.field.type)
				movedFields.add(use.field)
				info.typeModifiers.uninitFields.update(movedFields)
			else:
				use.typeModifiers = info.typeModifiers.clone()
		elif use.rvalue.typeModifiers:
			if use.field:
				initFields = allFields(use.field.type)
				initFields.difference_update(use.rvalue.typeModifiers.uninitFields)
				info.typeModifiers.uninitFields.difference_update(initFields)
				info.typeModifiers.uninitFields.discard(use.field)
			else:
				info.typeModifiers = use.rvalue.typeModifiers.clone()
		
		if info.returnsSinceLastUse:
			for ret in info.returnsSinceLastUse:
				self.dropSymbol(info.symbol, ret.dropBlock)
			info.returnsSinceLastUse = set()
		
		if info.breaksSinceLastUse and info.wasDeclared:
			for (br, loopExpr) in info.breaksSinceLastUse.items():
				if self.loopExpr == loopExpr:
					self.dropSymbol(info.symbol, br.dropBlock)
			info.breaksSinceLastUse = {}
		
		if info.dropInBlock:
			info.dropInBlock = set()
		
		if info.usesOfBorrowsSinceLastUse:
			for borrowUse in info.usesOfBorrowsSinceLastUse:
				borrowUse.isBorrowed = True
			info.usesOfBorrowsSinceLastUse = set()
		
		if info.borrowedBy and use.symbol == info.symbol:
			borrowedBy = set()
			for borrower in info.borrowedBy:
				if borrower in self.symbolInfo:
					borrowedBy.add(borrower)
					borrowerInfo = self.symbolInfo[borrower]
					borrowerInfo.usesOfBorrowsSinceLastUse.add(use)
			info.borrowedBy = borrowedBy
		
		if info.borrows:
			use.borrows = info.borrows
			scopeErrCount = 0
			for borrow in info.borrows:
				borrowInfo = self.loadAndSaveSymbolInfo(borrow.symbol)
				if borrowInfo == None:# not in self.symbolInfo:
					if scopeErrCount == 0:
						logError(self.state, use.lvalueSpan, 'borrowed value has gone out of scope')
					scopeErrCount += 1
					countStr = '' if scopeErrCount < 2 else '({}) '.format(scopeErrCount)
					logExplain(self.state, borrow.span, 'borrow {}originally occurred here'.format(countStr))
				elif info.symbol != borrow.symbol:
					# borrowInfo = self.symbolInfo[borrow.symbol]
					borrowInfo.borrowedBy.add(info.symbol)
					self.setLastUse(borrowInfo, use, isRead)
		
		info.lastUses = { use: self.loopExpr }
	
	def addrSymbol(self, expr, info):
		symbol = expr.symbol
		field = expr.field
		fieldSpan = expr.fieldSpan
		isIndex = len(expr.dynOffsets) > 0
		
		if expr.type.mut and not expr.symbol.mut:
			logError(self.state, expr.span, 'mutable borrow of immutable symbol')
		
		symbol.fixed = True
		self.setLastUse(info, expr, isRead=True)
	
	def readSymbol(self, expr, info):
		symbol = expr.symbol
		field = expr.field
		fieldSpan = expr.fieldSpan
		isIndex = len(expr.dynOffsets) > 0
		isField = expr.isFieldAccess
		
		if info.moved:
			if not info.symbol.type.isCopyable:
				maybeText = 'may have' if info.maybeMoved else 'has'
				logError(self.state, expr.span, 'the value in `{}` {} been moved'.format(symbol.name, maybeText))
				logExplain(self.state, list(info.lastUses)[0].span, '`{}` was moved here'.format(symbol.name))
				return
		elif info.uninit:
			maybeText = 'may not have' if info.maybeUninit else 'has not'
			logError(self.state, expr.span, '`{}` {} been initialized'.format(symbol.name, maybeText))
			return
		
		fieldInfo = None
		if field and not expr.deref:
			if field not in info.fieldInfo:
				uninit = info.uninit or info.typeModifiers and field in info.typeModifiers.uninitFields
				fieldInfo = FieldInfo(field, uninit)
				info.fieldInfo[field] = fieldInfo
			else:
				fieldInfo = info.fieldInfo[field]
			
			if fieldInfo.moved:
				maybeText = 'may have' if fieldInfo.maybeMoved else 'has'
				logError(self.state, fieldSpan, 'the value in field `{}` {} been moved'.format(field.name, maybeText))
				return
			elif fieldInfo.uninit:
				maybeText = 'may not have' if fieldInfo.maybeUninit else 'has not'
				logError(self.state, fieldSpan, 'the field `{}` {} been initialized'.format(field.name, maybeText))
				return
		
		self.setLastUse(info, expr, isRead=True)
		
		if expr.noop:
			pass
		elif isField:
			if fieldInfo:
				fieldInfo.moved = not field.type.isCopyable
				fieldInfo.uninit = fieldInfo.moved
		elif isIndex:
			pass
		else:
			info.moved = not expr.copy
			info.typeModifiers.uninit = info.moved
	
	def derefWriteSymbol(self, expr, info):
		self.readSymbol(expr, info)
		
		symbol = expr.symbol
		isField = expr.isFieldAccess
		field = expr.field
		fieldSpan = expr.fieldSpan
		isIndex = len(expr.dynOffsets) > 0
		
		if symbol.type.isPtrType and not symbol.type.mut:
			logError(self.state, expr.lvalueSpan, 'assignment target is not mutable')
		
		if info.borrows:
			for borrow in info.borrows:
				if borrow.symbol in self.symbolInfo:
					borrowInfo = self.symbolInfo[borrow.symbol]
					borrowInfo.borrowedBy.remove(info.symbol)
		
		info.borrows = expr.rvalue.borrows
		
		if isField:
			pass
		else:
			self.dropInd(symbol, expr.dropBeforeAssignBlock, expr.deref)
		
		# assert expr.borrows
		# for borrow in expr.borrows:
		# 	borrowInfo = self.loadAndSaveSymbolInfo(borrow.symbol)
		# 	if borrowInfo == None:
		# 		continue
		# 	self.writeSymbol(borrow, borrowInfo)
	
	def writeSymbol(self, expr, info, typeModifiers=None):
		symbol = expr.symbol
		field = expr.field
		isField = expr.isFieldAccess
		fieldSpan = expr.fieldSpan
		isIndex = len(expr.dynOffsets) > 0
		
		if info.borrows:
			for borrow in info.borrows:
				if borrow.symbol in self.symbolInfo:
					borrowInfo = self.symbolInfo[borrow.symbol]
					borrowInfo.borrowedBy.remove(info.symbol)
		
		info.borrows = expr.rvalue.borrows
		
		if not symbol.mut and (not info.uninit or info.moved):
			logError(self.state, expr.lvalueSpan, 'assignment target is not mutable')
			return
		elif isIndex or field:
			if field not in info.fieldInfo:
				fieldInfo = FieldInfo(field, False)
				info.fieldInfo[field] = fieldInfo
			else:
				fieldInfo = info.fieldInfo[field]
			
			if info.moved:
				if not info.symbol.type.isCopyable:
					maybeText = 'may have' if info.maybeMoved else 'has'
					logError(self.state, expr.lvalueSpan, 'the value in `{}` {} been moved'.format(symbol.name, maybeText))
					logExplain(self.state, list(info.lastUses)[0].span, '`{}` was moved here'.format(symbol.name))
					return
			elif info.uninit:
				maybeText = 'may not have' if info.maybeUninit else 'has not'
				logError(self.state, expr.lvalueSpan, '`{}` {} been initialized'.format(symbol.name, maybeText))
				return
		
		self.setLastUse(info, expr, isRead=False)
		
		if not info.uninit and symbol.dropFn:
			self.dropSymbol(symbol, expr.dropBeforeAssignBlock)
		
		if typeModifiers:
			info.typeModifiers = typeModifiers.clone()
		
		info.maybeUninit = False
		info.moved = False
		info.maybeMoved = False
		
		if isField:
			if fieldInfo:
				fieldInfo.moved = False
				fieldInfo.uninit = False
		elif isIndex:
			pass
		else:
			info.typeModifiers.uninit = False
	
	def callDropFn(self, dropFn, symbol, field, fieldBase, exprs, span, indLevel=0):
		from .mir.access import SymbolRead
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
		exprs.append(fnCall)
	
	def dropEnum(self, symbol, field, fieldBase, exprs, span, indLevel=0):
		symbolType = symbol.type.typeAfterDeref(indLevel) if indLevel > 0 else symbol.type
		t = field.type if field else symbolType
		assert t.isEnumType
		parentDropFn = field.type.dropFn if field else symbol.dropFn
		
		for variant in t.variants:
			if variant.type.isCompositeType:
				for field in variant.type.fields:
					if field.type.isOwnedType and not parentDropFn:
						logError(self.state, symbol.span, 
							'owned value in field `{}` was not discarded'.format(field.name))
						logExplain(self.state, span.endSpan(), 'value escapes here')
					elif field.type.dropFn:
						logWarning(state, symbol.span, 
							'I can\'t drop `enum`s properly; field `{}` will not be dropped'.format(field.name))
	
	def dropField(self, symbol, field, block, span):
		fieldInfo = self.symbolInfo[symbol].fieldInfo
		if field not in fieldInfo or not fieldInfo[field].uninit:
			if field.type.isOwnedType:
				logError(self.state, symbol.span, 
					'owned value in field `{}` was not discarded'.format(field.name))
				logExplain(self.state, span, 'value escapes here')
			
			if field.type.dropFn:
				self.callDropFn(field.type.dropFn, symbol, field, fieldBase, block.exprs, span)
			
			self.dropFields(symbol, field, field.offset, block.exprs, span)
	
	def dropFields(self, symbol, field, fieldBase, exprs, span, indLevel=0):
		symbolType = symbol.type.typeAfterDeref(indLevel) if indLevel > 0 else symbol.type
		t = field.type if field else symbolType
		parentDropFn = field.type.dropFn if field else symbol.dropFn
		
		if t.isEnumType:
			self.dropEnum(symbol, field, fieldBase, exprs, span, indLevel)
			return
		elif not t.isCompositeType:
			return
		
		fieldInfo = self.symbolInfo[symbol].fieldInfo
		for field in reversed(t.fields):
			if field not in fieldInfo or not fieldInfo[field].uninit:
				if field.type.isOwnedType and not parentDropFn:
					logError(self.state, symbol.span, 
						'owned value in field `{}` was not discarded'.format(field.name))
					logExplain(self.state, span.endSpan(), 'value escapes here')
				
				if field.type.dropFn:
					self.callDropFn(field.type.dropFn, symbol, field, fieldBase, exprs, span, indLevel)
				
				self.dropFields(symbol, field, fieldBase + field.offset, exprs, span, indLevel)
	
	def dropSymbol(self, symbol, block):
		exprs = []
		
		if symbol.type.isOwnedType:
			logError(self.state, symbol.span, 'owned value was not discarded')
			logExplain(self.state, block.span.endSpan(), 'value escapes here')
		
		if symbol.dropFn:
			self.callDropFn(symbol.dropFn, symbol, None, 0, exprs, block.span)
		
		self.dropFields(symbol, None, 0, exprs, block.span)
		
		exprs.append(DropSymbol(symbol, block.span))
		block.exprs[:0] = exprs
	
	def dropInd(self, symbol, block, indLevel):
		exprs = []
		
		# if symbol.type.isOwnedType:
		# 	logError(self.state, symbol.span, 'owned value was not discarded')
		# 	logExplain(self.state, block.span.endSpan(), 'value escapes here')
		
		if symbol.type.baseType.dropFn:
			self.callDropFn(symbol.type.baseType.dropFn, symbol, None, 0, exprs, block.span, indLevel=indLevel)
		
		self.dropFields(symbol, None, 0, exprs, block.span, indLevel=indLevel)
		
		block.exprs[:0] = exprs

	