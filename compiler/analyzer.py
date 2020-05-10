import ctypes
from .typeref    import NamedTypeRef, PtrTypeRef, ArrayTypeRef, TupleTypeRef
from .types      import Type, FieldInfo, PtrType, ArrayType, TupleType, Void, Bool, Byte, Char, Int8, \
                        UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, ISize, USize, Float32, Float64
from .log        import logError, logExplain
from .ast        import ASTPrinter, TypeSymbol
from .scope      import Scope, ScopeType
from .attrs      import invokeAttrs
from .mod        import Mod
from .fndecl     import FnDecl, CConv
from .staticdecl import StaticDecl, ConstDecl
from .structdecl import StructDecl

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

def analyzeTupleTypeRef(state, tupleTypeRef):
	resolvedTypes = [state.resolveTypeRef(t) for t in tupleTypeRef.types]
	layout = state.generateFieldLayout(resolvedTypes)
	tupleTypeRef.type = TupleType(layout.align, layout.byteSize, layout.fields)

def analyzeStaticDeclSig(state, decl):
	if decl.typeRef:
		decl.type = state.resolveTypeRef(decl.typeRef)

def analyzeStaticDeclBody(state, decl):
	analyzeConstDeclBody(state, decl)
	decl.mangledName = mangleName(state, decl)

def buildSymbolTable(state, mod):
	symbolTable = {}
	for decl in mod.decls:
		if decl.name in symbolTable:
			otherDecl = symbolTable[decl.name]
			logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
			logExplain(state, otherDecl.nameTok.span, '`{}` previously declared here'.format(decl.name))
		else:
			symbolTable[decl.name] = decl
		
		if type(decl) == StructDecl:
			mod.structs.append(decl)
		elif type(decl) == FnDecl:
			mod.fns.append(decl)
			if decl.name == 'main':
				mod.mainFn = decl
		elif type(decl) == StaticDecl:
			mod.statics.append(decl)
		elif type(decl) == ConstDecl:
			mod.consts.append(decl)
		# elif type(decl) == ImportDecl:
		# 	mod.imports.append(decl)
		elif type(decl) == Mod:
			mod.mods.append(decl)
			decl.symbolTable = buildSymbolTable(state, decl)
	
	return symbolTable

def analyze(ast):
	state = AnalyzerState()
	
	ast.symbolTable = buildSymbolTable(state, ast)
	
	state.pushScope(ScopeType.MOD, name=ast.name)
	state.scope.setSymbolTable(ast.symbolTable)
	
	ast.analyzeSig(state)
	ast = state.analyzeNode(ast)
	
	state.popScope()
	
	# p = ASTPrinter()
	# ast.pretty(p)
	
	if state.failed:
		exit(1)
	
	return ast

class AnalyzerState:
	def __init__(self):
		self.lastIfBranchOuterSymbolInfo = None
		self.scope = None
		self.failed = False
	
	def analyzeNode(state, ast, implicitType=None):
		invokeAttrs(state, ast)
		ast = ast.lower(state)
		newAST = ast.analyze(state, implicitType)
		return newAST if newAST else ast
	
	def resolveTypeRef(state, typeRef):
		if type(typeRef) == NamedTypeRef:
			return state.lookupSymbol(typeRef, True)
		elif type(typeRef) == PtrTypeRef:
			baseType = state.resolveTypeRef(typeRef.baseType)
			if baseType == None:
				return None
			return PtrType(baseType, typeRef.indLevel)
		elif type(typeRef) == ArrayTypeRef:
			baseType = state.resolveTypeRef(typeRef.baseType)
			if baseType == None:
				return None
			return ArrayType(baseType, typeRef.count)
		elif type(typeRef) == TupleTypeRef:
			return analyzeTupleTypeRef(state, typeRef)
		else:
			assert 0
	
	def mangleName(state, decl):
		if type(decl) == FnDecl and decl.cconv == CConv.C:
			return '_{}'.format(decl.name)
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
	
	def generateFieldLayout(state, types, fieldNames=None):
		fields = []
		maxAlign = 0
		offset = 0
		
		if fieldNames == None:
			fieldNames = (str(i) for i in range(0, len(types)))
		
		for (t, n) in zip(types, fieldNames):
			if t.isVoidType:
				continue
			
			maxAlign = max(maxAlign, t.align)
			
			if offset % t.align > 0:
				offset += t.align - offset % t.align
			
			fields.append(FieldInfo(n, t, offset))
			offset += t.byteSize
		
		return FieldLayout(maxAlign, offset, fields)
	
	def pushScope(self, scopeType, name=None, fnDecl=None, ifExpr=None, loopExpr=None):
		self.scope = Scope(
			self, 
			self.scope, 
			scopeType, 
			name=name, 
			fnDecl=fnDecl,
			loopExpr=loopExpr,
			ifExpr=ifExpr,
			ifBranchOuterSymbolInfo=self.lastIfBranchOuterSymbolInfo)
		
		self.lastIfBranchOuterSymbolInfo = None
	
	def popScope(self):
		outerSymbolInfo = self.scope.finalize()
		oldScopeType = self.scope.type
		
		self.scope = self.scope.parent
		
		if oldScopeType == ScopeType.IF:
			self.lastIfBranchOuterSymbolInfo = outerSymbolInfo
		elif oldScopeType not in (ScopeType.FN, ScopeType.MOD):
			for info in outerSymbolInfo.values():
				if info.symbol in self.scope.symbolInfo:
					# info = info.clone()
					info.wasDeclared = self.scope.symbolInfo[info.symbol].wasDeclared
				self.scope.symbolInfo[info.symbol] = info
	
	def lookupSymbol(self, ref, inTypePosition=False):
		symbolTok = ref.path[0]
		path = ref.path[1:]
		
		if symbolTok.content in BUILTIN_TYPES:
			symbol = BUILTIN_TYPES[symbolTok.content]
		else:
			symbol = self.scope.lookupSymbol(symbolTok.content)
		
		if symbol != None:
			for tok in path:
				# if type(symbol) == StructDecl:
				# 	symbol = symbol.resolvedSymbolType.fieldDict[tok.content]
				# el
				if type(symbol) == Mod:
					symbolTok = tok
					if tok.content not in symbol.symbolTable:
						symbol = None
						break
					
					symbol = symbol.symbolTable[tok.content]
				else:
					logError(self, symbolTok.span, '`{}` is not a module'.format(symbol.name))
					return None
		
		if symbol and inTypePosition and isinstance(symbol, TypeSymbol):
			symbol = symbol.type
		
		if inTypePosition:
			if symbol == None:
				logError(self, symbolTok.span, 'cannot resolve type `{}`'.format(symbolTok.content))
			elif not isinstance(symbol, Type):
				logError(self, symbolTok.span, '`{}` is not a type'.format(symbolTok.content))
				symbol = None
		else:
			if symbol == None:
				logError(self, symbolTok.span, 'cannot resolve the symbol `{}`'.format(symbolTok.content))
			elif isinstance(symbol, Type):
				logError(self, symbolTok.span, 'found a type reference where a value was expected')
				symbol = None
			elif type(symbol) == Mod:
				logError(self, symbolTok.span, 'found a module name where a value was expected')
				symbol = None
		
		return symbol
