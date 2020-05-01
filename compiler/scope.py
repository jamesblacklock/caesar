from enum       import Enum
from .ast       import FnParam, StaticDecl
from .drop      import DropSymbol
from .log       import logError, logWarning, logExplain
from .          import asgn as asgnmod, valueref, fndecl, constdecl, letdecl, field as fieldmod
from .fncall    import FnCall

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

# class UseInfo:
# 	def __init__(self, loopExpr, isWrite):
# 		self.loopExpr = loopExpr
# 		self.isWrite = isWrite

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
		self.laterReturns = set()
		
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
			self.uninit = False
			self.maybeUninit = False
		elif type(symbol) == letdecl.LetDecl:
			self.uninit = True
			self.maybeUninit = False
			# if symbol.resolvedSymbolType.isCompositeType:
			# 	addFieldInfo(symbol.resolvedSymbolType.fields, symbol.expr)
		else:
			assert 0
	
	def clone(self):
		info = SymbolInfo(self.symbol)
		info.wasDeclared = False
		info.lastUses = self.lastUses
		info.dropInBlock = self.dropInBlock
		info.laterReturns = self.laterReturns
		info.fieldInfo = { k: v.clone() for (k, v) in self.fieldInfo.items() }
		info.moved = self.moved
		info.maybeMoved = self.maybeMoved
		info.uninit = self.uninit
		info.maybeUninit = self.maybeUninit
		
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
						logWarning(self, info.symbol.span, 'unused symbol')
					elif type(info.symbol) == FnParam:
						self.dropSymbol(info.symbol, info.symbol.dropBlock)
				else:
					for block in info.dropInBlock:
						self.dropSymbol(info.symbol, block)
					for (lastUse, loopExpr) in info.lastUses.items():
						if type(lastUse) in (asgnmod.Asgn, fieldmod.Field):
							if loopExpr and loopExpr != self.loopExpr:
								for loopBreak in loopExpr.breaks:
									self.dropSymbol(info.symbol, loopBreak.block)
							else:
								self.dropSymbol(info.symbol, lastUse.dropBlock)
						else:
							if loopExpr and loopExpr != self.loopExpr and info.symbol.type.isCopyable:
								for loopBreak in loopExpr.breaks:
									self.dropSymbol(info.symbol, loopBreak.block)
							elif info.symbol.dropFn:
								assert 0
								# self.dropSymbol(info.symbol, lastUse.dropBlock)
			else:
				outerSymbolInfo[info.symbol] = info
		
		if self.type == ScopeType.FN:
			pass
			# for ret in self.fnDecl.returns:
			# 	for symbol in ret.dropSymbols:
			# 		if not self.symbolInfo[symbol].uninit:
			# 			self.dropSymbol(symbol, ret.block)
		elif self.type == ScopeType.ELSE:
			ifInfo = self.ifBranchOuterSymbolInfo
			elseInfo = outerSymbolInfo
			symbols = set(ifInfo.keys())
			symbols.update(elseInfo.keys())
			for symbol in symbols:
				if symbol in ifInfo and symbol in elseInfo:
					info1 = ifInfo[symbol]
					info2 = elseInfo[symbol]
					info1.lastUses.update(info2.lastUses)
					if info2.maybeMoved or info1.moved != info2.moved:
						info1.moved = True
						info1.maybeMoved = True
					if info2.maybeUninit or info1.uninit != info2.uninit:
						info.uninit = True
						info.maybeUninit = True
				else:
					if symbol in ifInfo:
						info = ifInfo[symbol]#.clone()
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
						info.uninit = True
						info.maybeUninit = True
		
		return outerSymbolInfo
	
	def doReturn(self, symbol, ret):
		self.didReturn = True
		if self.type == ScopeType.FN:
			return
		
		scope = self.parent
		while True:
			for info in scope.symbolInfo.values():
				if info.symbol == symbol or info.uninit:
					if info.symbol.type and info.symbol.type.isCopyable:
						info.laterReturns.add(ret)
				else:
					self.dropSymbol(info.symbol, ret.block)
			
			if scope.type == ScopeType.FN:
				break
			
			scope = scope.parent
	
	def declSymbol(self, symbol):
		info = SymbolInfo(symbol)
		self.symbolInfo[symbol] = info
		self.symbolTable[symbol.name] = symbol
	
	def addrSymbol(self, addr, fieldAccess=None):
		pass
	
	def readSymbol(self, expr):
		ref = None
		fieldAccess = None
		if type(expr) == fieldmod.Field:
			fieldAccess = expr
			ref = fieldAccess.expr
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
		
		if info.moved and symbol.type.isCopyable:
			for lastUse in info.lastUses:
				if type(lastUse) == valueref.ValueRef:
					lastUse.copy = True
			
			for ret in info.laterReturns:
				self.dropSymbol(info.symbol, ret.block)
			
			info.moved = False
			info.maybeMoved = False
			info.laterReturns = set()
		elif info.moved:
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
		
		info.lastUses = { expr: self.loopExpr }
		info.dropInBlock = set()
		if fieldAccess:
			fieldInfo.moved = True#not fieldAccess.field.type.isCopyable
		else:
			# ref.copy = symbol.type.isCopyable
			info.moved = True#not ref.copy
			info.uninit = True#info.moved
	
	def writeSymbol(self, asgn, field=None, typeModifiers=None):
		ref = asgn.lvalue
		assert type(ref) == valueref.ValueRef
		
		symbol = ref.symbol
		if symbol not in self.symbolInfo:
			assert 0#return # not a local variable
		
		info = self.symbolInfo[symbol]
		if not symbol.mut and not info.uninit:
			logError(self.state, ref.span, 'assignment target is not mutable')
			return
		
		if not info.uninit and symbol.dropFn:
			self.dropSymbol(symbol, asgn.dropBeforeAssignBlock)
		elif not info.uninit or info.maybeUninit:
			for (lastUse, loopExpr) in info.lastUses.items():
				if type(lastUse) == asgnmod.Asgn:
					self.dropSymbol(symbol, lastUse.dropBlock)
				# elif loopExpr and loopExpr != self.loopExpr:
				# 	for loopBreak in loopExpr.breaks:
				# 		self.dropSymbol(info.symbol, loopBreak.block)
				# else:
					# lastUse.copy = False
		
		info = self.symbolInfo[symbol]
		info.lastUses = { asgn: self.loopExpr }
		info.dropInBlock = set()
		info.uninit = False
		info.maybeUninit = False
		info.moved = False
		info.maybeMoved = False
		if typeModifiers:
			info.typeModifiers = typeModifiers.clone()
		else:
			info.typeModifiers.uninit = False
		# if field:
		# 	info.fieldInfo[field].uninit = False
	
	def dropSymbol(self, symbol, block, prepend=True):
		# if symbol not in self.symbolInfo:
		# 	scope = self.parent
		# 	while True:
		# 		if symbol in scope.symbolInfo:
		# 			self.symbolInfo[symbol] = scope.symbolInfo[symbol].clone()
		# 			break
				
		# 		scope = scope.parent
		
		valueWasMoved = False
		exprs = []
		if symbol.dropFn:
			fnRef = valueref.ValueRef(None, None, name=symbol.dropFn.name)
			fnRef.symbol = symbol.dropFn
			args = []
			if len(symbol.dropFn.params) > 0:
				t = symbol.dropFn.params[0].type
				ref = valueref.ValueRef(None, None, name=symbol.name)
				ref.symbol = symbol
				if t.isPtrType and typesMatch(t.baseType, symbol.type):
					args.append(Address(ref, None))
				else:
					args.append(ref)
					valueWasMoved = True
			fnCall = FnCall(fnRef, args, None)
			fnCall.isDrop = True
			fnCall = self.state.analyzeNode(fnCall)
			if valueWasMoved:
				ref.copy = False
			exprs.append(fnCall)
		
		if not valueWasMoved:
			exprs.append(DropSymbol(symbol))
		
		if prepend:
			block.exprs.insert(0, *exprs)
		else:
			block.exprs.append(*exprs)
		
		# self.symbolInfo[symbol].uninit = True
	
	def lookupSymbol(self, name):
		scope = self
		while scope != None:
			if name in scope.symbolTable:
				symbol = scope.symbolTable[name]
				if symbol not in self.symbolInfo:
					if type(symbol) in (letdecl.LetDecl, FnParam):
						self.symbolInfo[symbol] = scope.symbolInfo[symbol].clone()
					elif type(symbol) == StaticDecl:
						info = SymbolInfo(symbol)
						info.wasDeclared = False
						self.symbolInfo[symbol] = info
				
				return symbol
			
			scope = scope.parent
		return None