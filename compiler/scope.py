from enum        import Enum
from .ast        import FnParam, StaticDecl
from .drop       import DropSymbol
from .log        import logError, logWarning, logExplain
from .           import asgn as asgnmod, valueref, fndecl, constdecl, letdecl, field as fieldmod, block as blockmod, address
from .fncall     import FnCall
from .structdecl import StructDecl

class ScopeType(Enum):
	MOD = "MOD"
	FN = "FN"
	LOOP = "LOOP"
	IF = "IF"
	ELSE = "ELSE"
	BLOCK = "BLOCK"

class FieldInfo:
	def __init__(self, field, uninit):
		self.field = field
		self.moved = False
		self.maybeMoved = False
		self.uninit = uninit
		self.maybeUninit = False
	
	def clone(self):
		info = FieldInfo(self.field, self.uninit)
		info.moved = self.moved
		info.maybeMoved = self.maybeMoved
		info.maybeUninit = self.maybeUninit
		return info

class SymbolInfo:
	def __init__(self, symbol):
		self.symbol = symbol
		self.wasDeclared = True
		self.lastUses = {}
		self.dropInBlock = set()
		self.moved = False
		self.maybeMoved = False
		self.fieldInfo = {}
		self.typeModifiers = symbol.typeModifiers.clone()
		self.returnsAfterMove = set()
		self.returnsSinceLastUse = set()
		self.breaksAfterMove = {}
		self.breaksSinceLastUse = {}
		self.borrows = set()
		
		
		# def addFieldInfo(fields, expr):
		# 	allInit = expr and type(expr) != StructLitAST
		# 	fieldDict = \
		# 		{ k: v for (k, v) in expr.fieldDict.items() } if type(expr) == StructLitAST else \
		# 		{ i: e for (i, e) in enumerate(expr.values) } if type(expr) in (TupleLitAST, ArrayLitAST) else \
		# 		{}
			
		# 	for field in fields:
		# 		self.fieldInfo[field] = FieldInfo(field, allInit or field.name in fieldDict)
		# 		fieldExpr = fieldDict[field.name] if field.name in fieldDict else None
		# 		if field.resolvedSymbolType.isStructType:
		# 			addFieldInfo(field.resolvedSymbolType.fields, fieldExpr)
		
		if type(symbol) in (StaticDecl, FnParam):
			self.typeModifiers.uninit = False
			self.maybeUninit = False
		elif type(symbol) == letdecl.LetDecl:
			self.typeModifiers.uninit = True
			self.maybeUninit = False
			# if symbol.resolvedSymbolType.isCompositeType:
			# 	addFieldInfo(symbol.resolvedSymbolType.fields, symbol.expr)
		else:
			assert 0
	
	@property
	def uninit(self):
		return self.typeModifiers.uninit
	
	def clone(self):
		info = SymbolInfo(self.symbol)
		info.wasDeclared = False
		info.lastUses = self.lastUses
		info.dropInBlock = self.dropInBlock
		info.returnsAfterMove = self.returnsAfterMove
		info.returnsSinceLastUse = self.returnsSinceLastUse
		info.breaksAfterMove = self.breaksAfterMove
		info.breaksSinceLastUse = self.breaksSinceLastUse
		info.fieldInfo = { k: v.clone() for (k, v) in self.fieldInfo.items() }
		info.moved = self.moved
		info.maybeMoved = self.maybeMoved
		info.maybeUninit = self.maybeUninit
		info.typeModifiers = self.typeModifiers.clone()
		return info

class Scope:
	def __init__(self, state, parent, scopeType, 
		name=None, fnDecl=None, loopExpr=None, ifExpr=None, ifBranchOuterSymbolInfo=None):
		self.state = state
		self.parent = parent
		self.type = scopeType
		self.symbolTable = {}
		self.symbolInfo = {}
		self.name = name
		self.didBreak = False
		self.didReturn = False
		self.fnDecl = fnDecl
		self.loopDepth = 0
		self.ifExpr = ifExpr
		self.loopExpr = loopExpr
		self.ifBranchOuterSymbolInfo = ifBranchOuterSymbolInfo
		self.dropBlock = None
		
		if parent:
			self.loopDepth = parent.loopDepth
			if not self.fnDecl: self.fnDecl = parent.fnDecl
			if not self.loopExpr: self.loopExpr = parent.loopExpr
		
		if self.loopDepth > 0 or scopeType == ScopeType.LOOP:
			self.loopDepth += 1
	
	def setSymbolTable(self, symbolTable):
		self.symbolTable = symbolTable
	
	def finalize(self):
		if self.type == ScopeType.MOD:
			return None
		
		outerSymbolInfo = {}
		for info in self.symbolInfo.values():
			if info.wasDeclared:
				if info.symbol.unused:
					if type(info.symbol) == letdecl.LetDecl:
						logWarning(self.state, info.symbol.span, 'unused symbol')
					elif type(info.symbol) == FnParam:
						self.dropSymbol(info.symbol, info.symbol.dropBlock)
				else:
					for block in info.dropInBlock:
						self.dropSymbol(info.symbol, block)
					for (lastUse, loopExpr) in info.lastUses.items():
						if type(lastUse) in (asgnmod.Asgn, fieldmod.Field, fieldmod.Index):
							if loopExpr and loopExpr != self.loopExpr:
								for (br, otherLoopExpr) in info.breaksSinceLastUse.items():
									if loopExpr == otherLoopExpr:
										self.dropSymbol(info.symbol, br.block)
							else:
								self.dropSymbol(info.symbol, lastUse.dropBlock)
						elif type(lastUse) == valueref.ValueRef:
							if lastUse.addr:
								assert 0
							elif lastUse.borrows:
								for borrow in lastUse.borrows:
									if borrow.symbol == info.symbol:
										self.dropSymbol(info.symbol, lastUse.dropBlock)
										break
							elif loopExpr and loopExpr != self.loopExpr and info.symbol.type.isCopyable:
								lastUse.copy = True
								for (br, otherLoopExpr) in info.breaksAfterMove.items():
									if loopExpr == otherLoopExpr:
										self.dropSymbol(info.symbol, br.block)
							elif info.symbol.dropFn and info.symbol.type.isCopyable:
								lastUse.copy = True
								self.dropSymbol(info.symbol, lastUse.dropBlock)
						else:
							assert 0
			else:
				outerSymbolInfo[info.symbol] = info
		
		if self.type == ScopeType.ELSE:
			ifInfo = self.ifBranchOuterSymbolInfo
			elseInfo = outerSymbolInfo
			symbols = set(ifInfo.keys())
			symbols.update(elseInfo.keys())
			for symbol in symbols:
				if symbol in ifInfo and symbol in elseInfo:
					info1 = ifInfo[symbol]
					info2 = elseInfo[symbol]
					info1.lastUses.update(info2.lastUses)
					info1.returnsAfterMove.update(info2.returnsAfterMove)
					info1.returnsSinceLastUse.update(info2.returnsSinceLastUse)
					info1.breaksAfterMove.update(info2.breaksAfterMove)
					info1.breaksSinceLastUse.update(info2.breaksSinceLastUse)
					info1.dropInBlock.update(info2.dropInBlock)
					info1.borrows.update(info2.borrows)
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
					
					if symbol not in self.parent.symbolInfo:
						self.parent.lookupSymbol(symbol.name)
					
					parentInfo = self.parent.symbolInfo[symbol]
					if parentInfo.moved != info.moved:
						info.moved = True
						info.maybeMoved = True
					if parentInfo.uninit != info.uninit:
						info.typeModifiers.uninit = True
						info.maybeUninit = True
		
		return outerSymbolInfo
	
	def doReturn(self, symbol, ret):
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
		
		inLoop = True
		scope = self.parent
		while True:
			for info in scope.symbolInfo.values():
				if not info.wasDeclared:
					continue
				
				if self.ifBranchOuterSymbolInfo and info.symbol in self.ifBranchOuterSymbolInfo:
					info = self.ifBranchOuterSymbolInfo[info.symbol]
				else:
					info = self.loadSymbolInfo(info.symbol)
				
				for loopExpr in info.lastUses.values():
					if loopExpr == self.loopExpr:
						if info.moved and info.symbol.type.isCopyable:
							info.breaksAfterMove[expr] = loopExpr
						elif not info.uninit:
							info.breaksSinceLastUse[expr] = loopExpr
						break
			
			if scope.type == stopAt:
				break
			
			scope = scope.parent
	
	def setLastUse(self, info, use, isRead):
		if info.moved and info.symbol.type.isCopyable and \
			(isRead or info.symbol.dropFn):
			for lastUse in info.lastUses:
				if type(lastUse) == valueref.ValueRef:
					lastUse.copy = True
			
			if info.returnsAfterMove:
				for ret in info.returnsAfterMove:
					self.dropSymbol(info.symbol, ret.block)
				info.returnsAfterMove = set()
				
			if info.breaksAfterMove:
				for (br, loopExpr) in info.breaksAfterMove.items():
					if self.loopExpr == loopExpr:
						self.dropSymbol(info.symbol, br.block)
				info.breaksAfterMove = {}
			
			info.moved = False
			info.maybeMoved = False
			info.typeModifiers.uninit = False
			info.maybeUninit = False
		
		if info.returnsSinceLastUse:
			for ret in info.returnsSinceLastUse:
				self.dropSymbol(info.symbol, ret.block)
			info.returnsSinceLastUse = set()
		
		if info.breaksSinceLastUse:
			for (br, loopExpr) in info.breaksSinceLastUse.items():
				if self.loopExpr == loopExpr:
					self.dropSymbol(info.symbol, br.block)
			info.breaksSinceLastUse = {}
		
		if info.dropInBlock:
			info.dropInBlock = set()
		
		info.lastUses = { use: self.loopExpr }
	
	def declSymbol(self, symbol):
		info = SymbolInfo(symbol)
		self.symbolInfo[symbol] = info
		self.symbolTable[symbol.name] = symbol
	
	def addrSymbol(self, addr):
		ref = None
		fieldAccess = None
		isIndex = False
		if type(addr.expr) == fieldmod.Field:
			fieldAccess = addr
			ref = fieldAccess.expr
		elif type(addr.expr) == fieldmod.Index:
			isIndex = True
			ref = addr.expr
		elif type(addr.expr) == valueref.ValueRef:
			ref = addr.expr
		else:
			assert 0
		
		assert type(ref) == valueref.ValueRef
		
		info = self.symbolInfo[ref.symbol]
		self.setLastUse(info, ref, True)
	
	def readSymbol(self, expr):
		ref = None
		fieldAccess = None
		isIndex = False
		if type(expr) == fieldmod.Field:
			fieldAccess = expr
			ref = fieldAccess.expr
		elif type(expr) == fieldmod.Index:
			isIndex = True
			ref = expr.expr
		elif type(expr) == valueref.ValueRef:
			ref = expr
		else:
			assert 0
		
		assert type(ref) == valueref.ValueRef
		
		fieldInfo = None
		symbol = ref.symbol
		if symbol not in self.symbolInfo:
			assert type(symbol) in (constdecl.ConstDecl, fndecl.FnDecl)
			return
		
		info = self.symbolInfo[symbol]
		
		if info.moved:
			if not info.symbol.type.isCopyable:
				maybeText = 'may have' if info.maybeMoved else 'has'
				logError(self.state, ref.span, 'the value in `{}` {} been moved'.format(ref.name, maybeText))
				logExplain(self.state, list(info.lastUses)[0].span, '`{}` was moved here'.format(ref.name))
				return
		elif info.uninit:
			maybeText = 'may not have' if info.maybeUninit else 'has not'
			logError(self.state, ref.span, '`{}` {} been initialized'.format(ref.name, maybeText))
			return
		elif fieldAccess:
			field = fieldAccess.field
			fieldSpan = fieldAccess.path[-1].span
			if field not in info.fieldInfo:
				uninit = info.typeModifiers.uninit or field in info.typeModifiers.uninitFields
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
		
		if info.borrows:
			expr.borrows = info.borrows
			errCount = 0
			for borrow in info.borrows:
				if borrow.symbol not in self.symbolInfo:
					if errCount == 0:
						logError(self.state, expr.span, 'borrowed value has gone out of scope')
					errCount += 1
					countStr = '' if errCount < 2 else '({}) '.format(errCount)
					logExplain(self.state, borrow.span, 'borrow {}originally occurred here'.format(countStr))
				else:
					self.setLastUse(self.symbolInfo[borrow.symbol], expr, isRead=True)
		
		if fieldInfo:
			fieldInfo.moved = True#not fieldAccess.field.type.isCopyable
		elif isIndex:
			pass
		else:
			info.moved = True
			info.typeModifiers.uninit = True
	
	def writeSymbol(self, asgn, borrowedSymbol=None, typeModifiers=None):
		ref = None
		fieldAccess = None
		isIndex = False
		if type(asgn.lvalue) == valueref.ValueRef:
			ref = asgn.lvalue
		elif type(asgn.lvalue) == fieldmod.Index:
			assert type(asgn.lvalue.expr) == valueref.ValueRef
			ref = asgn.lvalue.expr
			isIndex = True
		
		assert type(ref) == valueref.ValueRef
		
		symbol = ref.symbol
		assert symbol in self.symbolInfo
		
		info = self.symbolInfo[symbol]
		if not symbol.mut and not info.uninit:
			logError(self.state, ref.span, 'assignment target is not mutable')
			return
		elif isIndex or fieldAccess:
			if info.moved:
				if not info.symbol.type.isCopyable:
					maybeText = 'may have' if info.maybeMoved else 'has'
					logError(self.state, ref.span, 'the value in `{}` {} been moved'.format(ref.name, maybeText))
					logExplain(self.state, list(info.lastUses)[0].span, '`{}` was moved here'.format(ref.name))
					return
			elif info.uninit:
				maybeText = 'may not have' if info.maybeUninit else 'has not'
				logError(self.state, ref.span, '`{}` {} been initialized'.format(ref.name, maybeText))
				return
		
		if not info.uninit and symbol.dropFn:
			self.dropSymbol(symbol, asgn.dropBeforeAssignBlock)
		elif not info.uninit or info.maybeUninit:
			for (lastUse, loopExpr) in info.lastUses.items():
				if type(lastUse) == asgnmod.Asgn and type(lastUse.lvalue) == valueref.ValueRef:
					self.dropSymbol(symbol, lastUse.dropBlock)
		
		if typeModifiers:
			info.typeModifiers = typeModifiers.clone()
		
		info.maybeUninit = False
		info.moved = False
		info.maybeMoved = False
		info.borrows = asgn.rvalue.borrows
		
		if fieldAccess:
			assert 0
			info.fieldInfo[field].uninit = False
		elif isIndex:
			pass
		else:
			info.typeModifiers.uninit = False
	
	def dropSymbol(self, symbol, block, prepend=True):
		valueWasMoved = False
		exprs = []
		if symbol.dropFn:
			fnRef = valueref.ValueRef(None, None, name=symbol.dropFn.name)
			fnRef.symbol = symbol.dropFn
			fnRef.name = symbol.dropFn.name
			fnRef.type = symbol.dropFn.type
			args = []
			if len(symbol.dropFn.params) > 0:
				t = symbol.dropFn.params[0].type
				ref = valueref.ValueRef(None, None, name=symbol.name)
				ref.symbol = symbol
				ref.name = symbol.name
				ref.type = symbol.type
				if t.isPtrType and typesMatch(t.baseType, symbol.type):
					ref = Address(ref, None)
					ref.type = t
				else:
					valueWasMoved = True
				args.append(ref)
			fnCall = FnCall(fnRef, args, None)
			fnCall.isDrop = True
			fnCall.type = fnRef.type.returnType
			if not valueWasMoved:
				ref.copy = True
			exprs.append(fnCall)
		
		if not valueWasMoved:
			exprs.append(DropSymbol(symbol))
		
		if prepend:
			block.exprs.insert(0, *exprs)
		else:
			block.exprs.append(*exprs)
	
	def lookupSymbol(self, name):
		scope = self
		symbol = None
		while scope != None:
			if name in scope.symbolTable:
				symbol = scope.symbolTable[name]
				break
			
			scope = scope.parent
		
		if symbol == None:
			return None
		
		if type(symbol) in (letdecl.LetDecl, FnParam, StaticDecl):
			self.symbolInfo[symbol] = self.loadSymbolInfo(symbol, clone=True)
		
		return symbol
	
	def loadSymbolInfo(self, symbol, clone=False):
		assert type(symbol) in (letdecl.LetDecl, FnParam, StaticDecl)
		scope = self
		while scope != None:
			if symbol in scope.symbolInfo:
				info = scope.symbolInfo[symbol]
				return info.clone() if clone and scope != self else info
			scope = scope.parent
		
		assert type(symbol) == StaticDecl
		info = SymbolInfo(symbol)
		info.wasDeclared = False
		return info