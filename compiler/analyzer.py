import ctypes
from .symbol.symbol  import SymbolType, Deps
from .ast.typeref    import NamedTypeRef, PtrTypeRef, ArrayTypeRef, OwnedTypeRef
from .types          import FieldInfo, PtrType, ArrayType, OwnedType, \
                            Void, Bool, Byte, Char, Int8, UInt8, Int16, UInt16, Int32, UInt32, \
                            Int64, UInt64, ISize, USize, Float32, Float64, typesMatch, canCoerce, tryPromote
from .log            import logError, logExplain
from .ast.ast        import ValueSymbol, Attr
from .scope          import Scope, ScopeType
from .attrs          import invokeAttrs
from .symbol.mod     import Mod, Impl
from .symbol.trait   import Trait
from .symbol.fn      import Fn, CConv
from .symbol.static  import Static
from .ast.structdecl import StructDecl
from .ast.tupledecl  import TupleDecl
# from .symbol.alias   import AliasDecl
from .mir.access     import SymbolAccess, SymbolRead
from .ast.importexpr import Import
from .               import platform
from .mir.block      import Block, createDropBlock

BUILTIN_TYPES = {
	Void.name:    Void,
	Bool.name:    Bool,
	Byte.name:    Byte,
	Int8.name:    Int8,
	UInt8.name:   UInt8,
	Int16.name:   Int16,
	UInt16.name:  UInt16,
	Int32.name:   Int32,
	UInt32.name:  UInt32,
	Char.name:    Char,
	Int64.name:   Int64,
	UInt64.name:  UInt64,
	ISize.name:   ISize,
	USize.name:   USize,
	Float32.name: Float32,
	Float64.name: Float64
}

class FieldLayout:
	def __init__(self, align, byteSize, fields):
		self.align = align
		self.byteSize = byteSize
		self.fields = fields

def finishAnalyzingTupleType(state, resolvedType, deps):
	for field in resolvedType.fields:
		field.type = state.finishResolvingType(field.type, deps)
	return resolvedType

def finishAnalyzingStructType(state, resolvedType, deps):
	for field in resolvedType.fields:
		field.type = state.finishResolvingType(field.type, deps)
	return resolvedType

def analyzeOwnedTypeRefSig(state, ownedTypeRef):
	baseType = state.resolveTypeRefSig(ownedTypeRef.baseType)
	if not baseType.isCopyable:
		logError(state, ownedTypeRef.baseType.span, 'base type of owned type must be copyable')
	
	acquire = None
	release = None
	acquireSpan = ownedTypeRef.acquire.span if ownedTypeRef.acquire else ownedTypeRef.span
	releaseSpan = ownedTypeRef.release.span if ownedTypeRef.release else ownedTypeRef.span
	
	if ownedTypeRef.acquire:
		acquire = state.lookupSymbol(ownedTypeRef.acquire.path, inValuePosition=True)
		if acquire and type(acquire) != Fn:
			logError(state, ownedTypeRef.acquire.span, '`{}` must be a function'.format(acquire.name))
			acquire = None
	
	if ownedTypeRef.release:
		release = state.lookupSymbol(ownedTypeRef.release.path, inValuePosition=True)
		if release and type(release) != Fn:
			logError(state, ownedTypeRef.release.span, '`{}` must be a function'.format(release.name))
			release = None
	
	return OwnedType(baseType, acquire, release, acquireSpan, releaseSpan, ownedTypeRef.span)

def finishAnalyzingOwnedType(state, ownedType, deps):
	ok = True
	
	if not ownedType.acquire:
		if state.scope.acquireDefault:
			ownedType.acquire = state.scope.acquireDefault.symbol
		else:
			ok = False
			logError(state, ownedType.span, 'default `acquire` fn is not defined in the current scope')
	
	if not ownedType.release:
		if state.scope.releaseDefault:
			ownedType.release = state.scope.releaseDefault.symbol
		else:
			ok = False
			logError(state, ownedType.span, 'default `release` fn is not defined in the current scope')
	
	ownedType.updateName()
	
	if not ok:
		return
	
	if ownedType.acquire:
		ownedType.acquire.analyze(state, deps)
		returnType = ownedType.acquire.type.returnType
		if returnType and \
			not typesMatch(ownedType, returnType) and \
			not canCoerce(ownedType, returnType):
			logError(state, ownedType.acquireSpan, '`{}` must return `{}` (found `{}`)'.format(
				ownedType.acquire.name, ownedType.name, returnType.name))
	
	if ownedType.release:
		ownedType.release.analyze(state, deps)
		isValid = False
		for param in ownedType.release.params:
			if param.type and (typesMatch(ownedType, param.type) or canCoerce(ownedType, param.type)):
				isValid = True
				break
		
		if not isValid:
			logError(state, ownedType.releaseSpan, '`{}` must take an argument of type `{}`'.format( 
				ownedType.release.name, ownedType.name))
	
	return ownedType

def buildSymbolTable(state, mod):
	for decl in mod.decls:
		if type(decl) == Attr:
			mod.attrs.append(decl)
			continue
		elif type(decl) == Import:
			decl.importSymbols(state, mod)
			continue
		
		symbol = decl.createSymbol(state)
		if type(symbol) == Trait:
			buildSymbolTable(state, symbol.mod)
		elif type(symbol) == Fn:
			mod.fns.append(symbol)
			if symbol.name == 'main':
				mod.mainFn = symbol
		elif type(symbol) == Static:
			mod.statics.append(symbol)
		elif type(symbol) in (Mod, Impl):
			mod.mods.append(symbol)
			buildSymbolTable(state, symbol)
		
		if symbol.name in mod.symbolTable:
			otherSymbol = mod.symbolTable[symbol.name]
			logError(state, symbol.nameSpan, 'cannot redeclare `{}` as a different symbol'.format(symbol.name))
			logExplain(state, otherSymbol.nameSpan, '`{}` previously declared here'.format(symbol.name))
		else:
			mod.symbolTable[symbol.name] = symbol
			mod.symbols.append(symbol)

def analyze(ast):
	return AnalyzerState.analyze(None, ast)

class AnalyzerState:
	def __init__(self):
		# self.lastIfBranchOuterSymbolInfo = None
		self.scope = None
		self.failed = False
		self.strMod = None
		self.mirBlockStack = []
		self.ast = None
		self.discardLevel = 0
	
	@property
	def mirBlock(self):
		return self.mirBlockStack[-1]
	
	def analyze(_, ast):
		state = AnalyzerState()
		state.ast = ast
		
		buildSymbolTable(state, ast)
		
		invokeAttrs(state, ast)
		if not ast.noStrImport:
			(state.strMod, _) = Import.doImport(state, ast, ['str', 'str'])
		
		ast.checkSig(state)
		ast.analyze(state, Deps(ast))
		
		if state.failed:
			exit(1)
		
		return ast
	
	def analyzeNode(self, ast, implicitType=None, isWrite=False, discard=False):
		assert not ast.analyzed
		if discard:
			if self.discardLevel == 0:
				self.pushScope(ScopeType.BLOCK)
				self.scope.dropBlock = createDropBlock(ast)
			self.discardLevel += 1
		
		invokeAttrs(self, ast)
		mir = ast.analyze(self, implicitType)
		if mir:
			if self.scope.fnDecl == None:
				assert mir.hasValue
			elif mir.hasValue:
				if not isWrite and type(mir) != SymbolRead:
					mir = SymbolAccess.read(self, mir)
			else:
				self.mirBlock.append(mir)
				mir = None
		
		if discard:
			self.discardLevel -= 1
			if self.discardLevel == 0:
				self.popScope()
		elif self.discardLevel == 0:
			ast.setAnalyzed()
		
		return mir
	
	def resolveTypeRefSig(state, typeRef):
		if type(typeRef) == NamedTypeRef:
			builtinName = typeRef.path[0].content if len(typeRef.path) == 1 else None
			if builtinName in BUILTIN_TYPES:
				return BUILTIN_TYPES[builtinName]
			
			result = state.lookupSymbol(typeRef.path, inTypePosition=True)
			return result.type if result else None#UnknownType
		elif type(typeRef) == OwnedTypeRef:
			return analyzeOwnedTypeRefSig(state, typeRef)
		elif type(typeRef) == PtrTypeRef:
			baseType = state.resolveTypeRefSig(typeRef.baseType)
			return PtrType(baseType, typeRef.indLevel, typeRef.mut)
		elif type(typeRef) == ArrayTypeRef:
			baseType = state.resolveTypeRefSig(typeRef.baseType)
			return ArrayType(baseType, typeRef.count)
		elif type(typeRef) in (TupleDecl, StructDecl):
			symbol = typeRef.createSymbol(state)
			return symbol.type
		else:
			assert 0
	
	def finishResolvingType(state, resolvedType, deps=None):
		if deps == None:
			deps = Deps(None)
		
		if resolvedType == None:
			return None
		elif resolvedType.symbol:
			resolvedType.symbol.analyze(state, deps)
		elif resolvedType.isOwnedType:
			finishAnalyzingOwnedType(state, resolvedType, deps)
		elif resolvedType.isPtrType or resolvedType.isArrayType:# or resolvedType.isTypeDef:
			state.finishResolvingType(resolvedType.baseType, deps)
		elif resolvedType.isTupleType:
			finishAnalyzingTupleType(state, resolvedType, deps)
		elif resolvedType.isStructType:
			finishAnalyzingStructType(state, resolvedType, deps)
		
		resolvedType.updateName()
		return resolvedType
	
	def resolveTypeRef(state, typeRef):
		return state.finishResolvingType(state.resolveTypeRefSig(typeRef))
	
	def typeCheck(state, expr, expectedType):
		if not (expr.type and expectedType):
			return expr
		
		expr = tryPromote(state, expr, expectedType)
		if not typesMatch(expr.type, expectedType):
			logError(state, expr.span, 'expected type `{}`, found `{}`'.format(expectedType, expr.type))
		
		return expr
	
	def mangleName(state, decl):
		if type(decl) == Fn and decl.type.cconv == CConv.C:
			prefix = '_' if platform.MacOS or platform.Windows else ''
			return '{}{}'.format(prefix, decl.name)
		else:
			letter = None
			if type(decl) == Fn:
				letter = 'F'
			elif type(decl) == Static:
				letter = 'S'
			else:
				assert 0
			
			mangled = '{}{}{}'.format(letter, len(decl.name), decl.name)
			scope = state.scope
			while scope:
				if scope.type == ScopeType.MOD:
					mangled = 'M{}{}{}'.format(len(scope.name), scope.name, mangled)
				elif scope.type == ScopeType.FN:
					mangled = 'F{}{}{}'.format(len(scope.name), scope.name, mangled)
				else:
					assert 0
				
				scope = scope.parent
			
			return mangled
	
	def generateFieldLayout(state, types, fieldNames=None, fieldInfo=None):
		fields = []
		maxAlign = 0
		offset = 0
		
		if fieldNames == None:
			fieldNames = (str(i) for i in range(0, len(types)))
		if fieldInfo == None:
			fieldInfo = (None for _ in types)
		
		unionSize = 0
		
		for (t, n, f) in zip(types, fieldNames, fieldInfo):
			if t.isVoidType:
				continue
			elif t.byteSize == None:
				assert 0
			
			maxAlign = max(maxAlign, t.align)
			
			if offset % t.align > 0:
				offset += t.align - offset % t.align
			
			isUnionField = f.unionField if f else False
			noOffset     =   f.noOffset if f else False
			
			fields.append(FieldInfo(n, t, offset, isUnionField))
			
			if noOffset:
				unionSize = max(unionSize, t.byteSize)
			else:
				offset += max(unionSize, t.byteSize)
				unionSize = 0
		
		if unionSize > 0:
			offset += unionSize
		
		return FieldLayout(maxAlign, offset, fields)
	
	def pushMIR(self, block):
		self.mirBlockStack.append(block)
	
	def popMIR(self):
		return self.mirBlockStack.pop()
	
	def pushScope(self, scopeType, item=None):
		self.scope = Scope(self, self.scope, scopeType, item)
		
		# self.lastIfBranchOuterSymbolInfo = None
		self.pushMIR(Block(self.scope))
	
	def popScope(self):
		self.scope = self.scope.parent
		return self.popMIR()
	
	def lookupSymbol(self, path, symbolTable=None, inTypePosition=False, inValuePosition=False):
		symbolName = path[0]
		path = path[1:]
		
		if symbolName.content == '_':
			logError(self, symbolName.span, '`_` is not a valid symbol name')
			return None
		else:
			symbol = self.scope.lookupSymbol(symbolName.content)
		
		if symbol == None and symbolTable and symbolName.content in symbolTable:
			symbol = symbolTable[symbolName.content]
		
		if symbol != None:
			for name in path:
				if name.content == '_':
					logError(self, name.span, '`_` is not a valid symbol name')
					return None
				elif symbol.symbolType in (SymbolType.MOD, SymbolType.TYPE):
					symbolName = name
					if name.content not in symbol.symbolTable:
						symbol = None
						break
					
					parent = symbol
					symbol = symbol.symbolTable[name.content]
					if parent.isImport and not symbol.pub:
						symbol = None
				else:
					logError(self, symbolName.span, '`{}` is not a module'.format(symbol.name))
					return None
		
		# if type(symbol) == AliasDecl:
		# 	assert symbol.symbol
		# 	symbol = symbol.symbol
		
		if symbol == None:
			logError(self, symbolName.span, 'cannot resolve the symbol `{}`'.format(symbolName.content))
		elif inTypePosition and not inValuePosition:
			if symbol.symbolType not in (SymbolType.TYPE, SymbolType.VARIANT):
				logError(self, symbolName.span, '`{}` is not a type'.format(symbolName.content))
				symbol = None
		elif inValuePosition and not inTypePosition:
			if symbol.symbolType not in (SymbolType.VALUE, SymbolType.VARIANT):
				if symbol.symbolType == SymbolType.TYPE:
					found = 'type reference'
				elif symbol.symbolType == SymbolType.MOD:
					found = 'module'
				else:
					assert 0
				logError(self, symbolName.span, 'found {} where a value was expected'.format(found))
				symbol = None
		
		return symbol
