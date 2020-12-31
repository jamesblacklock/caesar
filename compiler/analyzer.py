from .symbol.symbol    import SymbolType, Deps
from .symbol.paramtype import ParamTypeInst
from .types            import BUILTIN_TYPES, typesMatch, canCoerce, tryPromote, FieldInfo
from .log              import logError, logExplain
from .ast.ast          import Attr
from .attrs            import invokeAttrs
from .symbol.mod       import Mod, Impl
from .symbol.trait     import Trait
from .symbol.fn        import Fn, CConv
from .symbol.static    import Static
# from .symbol.alias     import AliasDecl
from .ast.importexpr   import Import
from .                 import platform

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

def finishAnalyzingOwnedType(state, ownedType, deps):
	ok = True
	
	if not ownedType.acquire:
		if state.mod.acquireDefault:
			ownedType.acquire = state.mod.acquireDefault.symbol
		else:
			ok = False
			logError(state, ownedType.span, 'default `acquire` fn is not defined in the current scope')
	
	if not ownedType.release:
		if state.mod.releaseDefault:
			ownedType.release = state.mod.releaseDefault.symbol
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
			if symbol.name == 'main' and not state.isImport:
				mod.mainFn = symbol
		elif type(symbol) == Static:
			mod.statics.append(symbol)
		elif type(symbol) in (Mod, Impl):
			symbol.parent = mod
			mod.mods.append(symbol)
			buildSymbolTable(state, symbol)
			if type(symbol) == Impl:
				mod.symbols.append(symbol)
				continue
		
		if symbol.name in mod.symbolTable:
			otherSymbol = mod.symbolTable[symbol.name]
			logError(state, symbol.nameSpan, 'cannot redeclare `{}` as a different symbol'.format(symbol.name))
			logExplain(state, otherSymbol.nameSpan, '`{}` previously declared here'.format(symbol.name))
		else:
			mod.symbolTable[symbol.name] = symbol
			mod.symbols.append(symbol)

def analyze(ast, forceRebuilds=False, checkOnly=False):
	return AnalyzerState.analyze(None, ast, forceRebuilds)

class AnalyzerState:
	def __init__(self, ast, forceRebuilds, checkOnly, isImport):
		self.failed = False
		self.forceRebuilds = forceRebuilds
		self.checkOnly = checkOnly
		self.isImport = isImport
		self.ast = ast
		self.mod = None
	
	def analyze(self, ast, forceRebuilds=None, checkOnly=None, isImport=None):
		if forceRebuilds == None:
			forceRebuilds = self.forceRebuilds if self else False
		if checkOnly == None:
			checkOnly = self.checkOnly if self else False
		if isImport == None:
			isImport = self.isImport if self else False
		
		self = AnalyzerState(ast, forceRebuilds, checkOnly, isImport)
		
		buildSymbolTable(self, ast)
		
		invokeAttrs(self, ast)
		if not ast.noStrImport:
			(self.ast.strMod, _) = Import.doImport(self, ast, ['str', 'str'])
		elif ast.isStrMod:
			ast.strMod = ast
			ast.Str = ast.symbolTable['str']
			ast.StrData = ast.symbolTable['StrData']
		
		if not ast.noArrImport:
			(ast.arrMod, _) = Import.doImport(self, ast, ['arr', 'arr'])
		elif ast.isArrMod:
			ast.arrMod = ast
			ast.Arr = ast.symbolTable['arr']
			ast.Vec = ast.symbolTable['vec']
		
		ast.checkSig(self)
		ast.analyze(self, Deps(ast))
		
		if self.failed:
			exit(1)
		
		return ast
	
	def finishResolvingType(self, resolvedType, deps=None):
		if deps == None:
			deps = Deps(None)
		
		if resolvedType == None:
			return None
		elif resolvedType.symbol:
			resolvedType.symbol.analyze(self, deps)
		elif resolvedType.isOwnedType:
			finishAnalyzingOwnedType(self, resolvedType, deps)
		elif resolvedType.isPtrType or resolvedType.isArrayType:# or resolvedType.isTypeDef:
			self.finishResolvingType(resolvedType.baseType, deps)
		elif resolvedType.isTupleType:
			finishAnalyzingTupleType(self, resolvedType, deps)
		elif resolvedType.isStructType:
			finishAnalyzingStructType(self, resolvedType, deps)
		
		resolvedType.updateName()
		return resolvedType
	
	def resolveTypeRef(self, typeRef):
		return self.finishResolvingType(typeRef.resolveSig(self))
	
	def typeCheck(self, expr, expectedType):
		if not (expr.type and expectedType):
			return expr
		
		expr = tryPromote(self, expr, expectedType)
		if not typesMatch(expr.type, expectedType):
			logError(self, expr.span, 'expected type `{}`, found `{}`'.format(expectedType, expr.type))
		
		return expr
	
	def mangleName(self, decl):
		mod = self.mod
		
		if type(decl) == Fn and decl.type.cconv == CConv.C:
			prefix = '_' if platform.MacOS or platform.Windows else ''
			return '{}{}'.format(prefix, decl.name)
		else:
			letter = None
			if decl.symbolType == SymbolType.MOD:
				letter = 'M'
				mod = mod.parent
			elif decl.symbolType in (SymbolType.TYPE, SymbolType.PARAM_TYPE):
				letter = 'T'
			elif decl.isFn:
				letter = 'F'
			elif decl.isStatic:
				letter = 'S'
			else:
				assert 0
			
			mangled = '{}{}'.format(letter, decl.name)
			while mod:
				if not mod.transparent:
					if mod.isFnMod:
						mangled = 'F{}.{}'.format(mod.name, mangled)
					else:
						mangled = 'M{}.{}'.format(mod.name, mangled)
				
				mod = mod.parent
			
			return mangled
	
	def generateFieldLayout(self, types, fieldNames=None, fieldInfo=None):
		fields = []
		maxAlign = 0
		offset = 0
		
		if fieldNames == None:
			fieldNames = (str(i) for i in range(0, len(types)))
		if fieldInfo == None:
			fieldInfo = (None for _ in types)
		
		unionSize = 0
		sizeUnknown = False
		
		for (t, n, f) in zip(types, fieldNames, fieldInfo):
			sizeUnknown = sizeUnknown or t.byteSize == None
			
			if t.isVoidType:
				continue
			
			byteSize = t.byteSize if t.byteSize != None else 0
			align = t.align if t.align != None else 1
			
			maxAlign = max(maxAlign, align)
			
			if offset % align > 0:
				offset += align - offset % align
			
			isUnionField = f.unionField if f else False
			noOffset     =   f.noOffset if f else False
			pub          =        f.pub if f else True
			mut          =        f.mut if f else True
			
			fields.append(FieldInfo(n, t, offset, isUnionField, pub, mut))
			
			if noOffset:
				unionSize = max(unionSize, byteSize)
			else:
				offset += max(unionSize, byteSize)
				unionSize = 0
		
		if unionSize > 0:
			offset += unionSize
		
		byteSize = None if sizeUnknown else offset
		return FieldLayout(maxAlign, byteSize, fields)
	
	def lookupSymbol(self, path, symbolTable=None, implicitType=None, inTypePosition=False, inValuePosition=False):
		symbolName = path[0]
		path = path[1:]
		
		if symbolName.content == '_':
			logError(self, symbolName.span, '`_` is not a valid symbol name')
			return None
		elif not path and symbolName.content in BUILTIN_TYPES:
			assert not inTypePosition
			logError(self, symbolName.span, 'found type reference where a value was expected')
			return None
		elif symbolTable and symbolName.content in symbolTable:
			symbol = symbolTable[symbolName.content]
		else:
			mod = self.mod
			symbol = None
			while mod:
				if symbolName.content in mod.symbolTable:
					symbol = mod.symbolTable[symbolName.content]
					break
				mod = mod.parent
		
		if symbol == None and implicitType and implicitType.isEnumType and symbolName.content in implicitType.symbolTable:
			symbol = implicitType.symbolTable[symbolName.content]
		elif symbol.symbolType == SymbolType.PARAM_TYPE:
			if implicitType and implicitType.isStructType and implicitType.symbol.paramType:
				symbol = ParamTypeInst(implicitType.symbol.paramType, implicitType)
		
		if symbol != None:
			symbol.unused = False
			
			for name in path:
				if name.content == '_':
					logError(self, name.span, '`_` is not a valid symbol name')
					return None
				elif symbol.symbolType in (SymbolType.MOD, SymbolType.TYPE, SymbolType.PARAM_TYPE):
					symbolName = name
					if name.content not in symbol.symbolTable:
						symbol = None
						break
					
					parent = symbol
					symbol = symbol.symbolTable[name.content]
					symbol.unused = False
					if not symbol.pub:# and parent.isImport:
						symbol = None
						break
				else:
					logError(self, symbolName.span, '`{}` is not a module'.format(symbol.name))
					return None
		
		# if type(symbol) == AliasDecl:
		# 	assert symbol.symbol
		# 	symbol = symbol.symbol
		
		if symbol == None:
			logError(self, symbolName.span, 'cannot resolve the symbol `{}`'.format(symbolName.content))
		elif inTypePosition and not inValuePosition:
			if symbol.symbolType not in (SymbolType.TYPE, SymbolType.PARAM_TYPE, SymbolType.VARIANT):
				logError(self, symbolName.span, '`{}` is not a type'.format(symbolName.content))
				symbol = None
		elif inValuePosition and not inTypePosition:
			if symbol.symbolType not in (SymbolType.VALUE, SymbolType.VARIANT):
				if symbol.symbolType in (SymbolType.TYPE, SymbolType.PARAM_TYPE):
					found = 'type reference'
				elif symbol.symbolType == SymbolType.MOD:
					found = 'module'
				else:
					assert 0
				logError(self, symbolName.span, 'found {} where a value was expected'.format(found))
				symbol = None
		
		return symbol
