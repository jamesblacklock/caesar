import ctypes
from .not_done.typeref    import NamedTypeRef, PtrTypeRef, ArrayTypeRef, OwnedTypeRef
from .types               import TypeSymbol, UnknownType, FieldInfo, PtrType, ArrayType, OwnedType, \
                                 Void, Bool, Byte, Char, Int8, UInt8, Int16, UInt16, Int32, UInt32, \
                                 Int64, UInt64, ISize, USize, Float32, Float64, typesMatch, canCoerce, tryPromote
from .log                 import logError, logExplain
from .ast.ast             import ValueSymbol, Attr
from .scope               import Scope, ScopeType
from .ast.attrs           import invokeAttrs
from .not_done.mod        import Mod, Impl, TraitDecl
from .not_done.fndecl     import FnDecl, CConv
from .not_done.staticdecl import StaticDecl, ConstDecl
from .not_done.structdecl import StructDecl
from .not_done.tupledecl  import TupleDecl
from .not_done.alias      import AliasDecl, TypeDecl
from .not_done.enumdecl   import EnumDecl, VariantDecl
from .mir.access          import SymbolAccess, SymbolRead
from .ast.importexpr      import Import
from .                    import platform
from .mir.block           import Block, createDropBlock
from .mir.localsymbol     import LocalSymbol

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

def finishAnalyzingTupleType(state, resolvedType):
	for field in resolvedType.fields:
		field.type = state.finishResolvingType(field.type)
	return resolvedType

def finishAnalyzingStructType(state, resolvedType):
	for field in resolvedType.fields:
		field.type = state.finishResolvingType(field.type)
	return resolvedType

def analyzeOwnedTypeRefSig(state, ownedTypeRef):
	baseType = state.resolveTypeRefSig(ownedTypeRef.baseType)
	if not baseType.isCopyable:
		logError(state, ownedTypeRef.baseType.span, 'base type of owned type must be copyable')
	
	acquire = None
	release = None
	acquireSpan = ownedTypeRef.acquire.span if ownedTypeRef.acquire else ownedTypeRef.span
	releaseSpan = ownedTypeRef.release.span if ownedTypeRef.release else ownedTypeRef.span
	
	if ownedTypeRef.acquire == None:
		acquire = state.scope.acquireDefault
		if acquire == None:
			logError(state, ownedTypeRef.span, 'default `acquire` fn is not defined in the current scope')
	else:
		acquire = state.lookupSymbol(ownedTypeRef.acquire.path, inValuePosition=True)
		if acquire and type(acquire) != FnDecl:
			logError(state, ownedTypeRef.acquire.span, '`{}` must be a function'.format(acquire.name))
			acquire = None
	
	if ownedTypeRef.release == None:
		release = state.scope.releaseDefault
		if release == None:
			logError(state, ownedTypeRef.span, 'default `release` fn is not defined in the current scope')
	else:
		release = state.lookupSymbol(ownedTypeRef.release.path, inValuePosition=True)
		if release and type(release) != FnDecl:
			logError(state, ownedTypeRef.release.span, '`{}` must be a function'.format(release.name))
			release = None
	
	return OwnedType(baseType, acquire, release, acquireSpan, releaseSpan)

def finishAnalyzingOwnedType(state, ownedType):
	if ownedType.acquire:
		if ownedType.acquire.returnType and \
			not typesMatch(ownedType, ownedType.acquire.returnType) and \
			not canCoerce(ownedType, ownedType.acquire.returnType):
			logError(state, ownedType.acquireSpan, '`{}` must return `{}` (found `{}`)'.format(
				ownedType.acquire.name, ownedType.name, ownedType.acquire.returnType.name))
	
	if ownedType.release:
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
	symbolTable = {}
	for decl in mod.decls:
		if type(decl) == Import:
			mod.imports.append(decl)
			continue
		elif type(decl) == Attr:
			mod.attrs.append(decl)
			continue
		
		if decl.name in symbolTable:
			otherDecl = symbolTable[decl.name]
			logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
			logExplain(state, otherDecl.nameTok.span, '`{}` previously declared here'.format(decl.name))
		else:
			symbolTable[decl.name] = decl
		
		if type(decl) in (StructDecl, TupleDecl, AliasDecl, TypeDecl, TraitDecl, EnumDecl):
			mod.types.append(decl)
			if type(decl) == TraitDecl:
				decl.mod.symbolTable = buildSymbolTable(state, decl.mod)
		elif type(decl) == FnDecl:
			mod.fns.append(decl)
			if decl.name == 'main':
				mod.mainFn = decl
		elif type(decl) == StaticDecl:
			mod.statics.append(decl)
		elif type(decl) == ConstDecl:
			mod.consts.append(decl)
		elif type(decl) in (Mod, Impl):
			mod.mods.append(decl)
			decl.symbolTable = buildSymbolTable(state, decl)
		else:
			assert 0
	
	return symbolTable

def analyze(ast):
	return AnalyzerState.analyze(None, ast)

class T:
	def __init__(self, content, span):
		self.content = content
		self.span = span

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
		
		ast.symbolTable = buildSymbolTable(state, ast)
		
		invokeAttrs(state, ast)
		if not ast.noStrImport:
			(state.strMod, _) = Import.doImport(state, ast, ['str', 'str'])
		
		ast.analyzeSig(state)
		ast.analyze(state)
		
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
			result = state.lookupSymbol(typeRef.path, inTypePosition=True)
			return result if result else UnknownType
		elif type(typeRef) == OwnedTypeRef:
			return analyzeOwnedTypeRefSig(state, typeRef)
		elif type(typeRef) == PtrTypeRef:
			baseType = state.resolveTypeRefSig(typeRef.baseType)
			return PtrType(baseType, typeRef.indLevel, typeRef.mut)
		elif type(typeRef) == ArrayTypeRef:
			baseType = state.resolveTypeRefSig(typeRef.baseType)
			return ArrayType(baseType, typeRef.count)
		elif type(typeRef) in (TupleDecl, StructDecl):
			typeRef.analyzeSig(state)
			return typeRef
		else:
			assert 0
	
	def finishResolvingType(state, resolvedType):
		if resolvedType == None:
			return None
		elif resolvedType.isOwnedType:
			return finishAnalyzingOwnedType(state, resolvedType)
		elif resolvedType.isPtrType or resolvedType.isArrayType or resolvedType.isTypeDef:
			resolvedType.baseType = state.finishResolvingType(resolvedType.baseType)
			return resolvedType
		elif resolvedType.isTupleType:
			return finishAnalyzingTupleType(state, resolvedType)
		elif resolvedType.isStructType:
			return finishAnalyzingStructType(state, resolvedType)
		else:
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
		if type(decl) == FnDecl and decl.cconv == CConv.C:
			prefix = '_' if platform.MacOS or platform.Windows else ''
			return '{}{}'.format(prefix, decl.name)
		else:
			letter = None
			if type(decl) == FnDecl:
				letter = 'F'
			elif type(decl) == StaticDecl:
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
		symbolTok = path[0]
		path = path[1:]
		
		if symbolTok.content == '_':
			logError(self, symbolTok.span, '`_` is not a valid symbol name')
			return None
		elif symbolTok.content in BUILTIN_TYPES:
			symbol = BUILTIN_TYPES[symbolTok.content]
		else:
			symbol = self.scope.lookupSymbol(symbolTok.content)
		
		if symbol == None and symbolTable and symbolTok.content in symbolTable:
			symbol = symbolTable[symbolTok.content]
		
		if symbol != None:
			for tok in path:
				if tok.content == '_':
					logError(self, tok.span, '`_` is not a valid symbol name')
					return None
				elif type(symbol) == Mod or isinstance(symbol, TypeSymbol):
					symbolTok = tok
					if tok.content not in symbol.symbolTable:
						symbol = None
						break
					
					parent = symbol
					symbol = symbol.symbolTable[tok.content]
					if parent.extern and not symbol.pub:
						symbol = None
				else:
					logError(self, symbolTok.span, '`{}` is not a module'.format(symbol.name))
					return None
		
		if type(symbol) == AliasDecl:
			assert symbol.symbol
			symbol = symbol.symbol
		
		if symbol == None:
			logError(self, symbolTok.span, 'cannot resolve the symbol `{}`'.format(symbolTok.content))
		elif inTypePosition and not inValuePosition:
			if not isinstance(symbol, TypeSymbol) and type(symbol) != VariantDecl:
				logError(self, symbolTok.span, '`{}` is not a type'.format(symbolTok.content))
				symbol = None
		elif inValuePosition and not inTypePosition:
			if not isinstance(symbol, ValueSymbol) and type(symbol) != VariantDecl and type(symbol) != LocalSymbol:
				if isinstance(symbol, TypeSymbol):
					found = 'type reference'
				elif type(symbol) == Mod:
					found = 'module'
				else:
					assert 0
				logError(self, symbolTok.span, 'found {} where a value was expected'.format(found))
				symbol = None
		
		return symbol
