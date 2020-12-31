from .ast               import AST
from ..symbol.symbol    import Symbol, ValueSymbol, SymbolType, Deps
from ..symbol.fn        import Fn
from ..symbol.struct    import Struct
from ..mir.access       import SymbolAccess
from ..mir.mir          import StaticDataType
from ..log              import logError
from ..types            import typesMatch, Type, BUILTIN_TYPES

class GenericAssocConst(ValueSymbol):
	def __init__(self, param, staticValue, span):
		super().__init__(param.name.content, param.name.span, span, True)
		self.ast = param
		self.staticValue = staticValue
		self.isConst = True
		self.isGeneric = staticValue == None
		self.type = param.valueType
		self.contracts = {}
		if self.staticValue:
			assert self.staticValue.dataType == StaticDataType.INT
			self.mangledName = 'C{}'.format(len(str(self.staticValue.data)) + 1, self.staticValue.data)
		else:
			self.mangledName = 'C_'

class GenericAssocType(Symbol):
	def __init__(self, param, type, span):
		super().__init__(SymbolType.TYPE, param.name.content, param.name.span, span, True)
		self.ast = param
		self.type = type
	
	def analyze(self, state, deps):
		pass

class GenericType(Type):
	def __init__(self, name, span, symbol):
		super().__init__(name, span, symbol, isGenericType=True)
	
	def resolveGenerics(self, symbolTable):
		if self.name in symbolTable:
			return symbolTable[self.name].type
		return self

class GenericArg:
	def __init__(self, paramSymbol, argSymbol):
		self.paramSymbol = paramSymbol
		self.argSymbol = argSymbol

class GenericInst(AST):
	def __init__(self, path, args, span):
		super().__init__(span, True)
		self.path = path
		self.args = args
		self.inst = None
		self.mangledArgs = ''
		self.argInfo = []
		self.argSymbolTable = {}
	
	def analyze(self, state, implicitType):
		inst = self.constructFn(state)
		return SymbolAccess.readSymbol(inst, self.span)
	
	def buildArgs(self, state, genericSymbol, genericSymbolTable):
		genericParams = [None for _ in self.args]
		
		if genericSymbol:
			if len(genericSymbol.genericParams) != len(self.args):
				name = '::'.join(self.path)
				logError(state, self.span, ('`{}` instatiated with wrong number of arguments (expected {}, found {})').format(
					name, len(genericSymbol.genericParams), len(self.args)))
			
			for (i, param) in enumerate(genericSymbol.genericParams):
				genericParams[i] = param
		
		for (param, arg) in zip(genericParams, self.args):
			requireValue = param and param.valueType
			requireType = param and not param.valueType
			isValue = arg.hasValue or (arg.maybeValue and requireValue)
			
			if requireValue and not isValue:
				logError(state, arg.span, 'found type reference where a value was expected')
			elif requireType and isValue:
				logError(state, arg.span, 'found value expression where a type was expected')
			
			if isValue:
				state.beginScope(self.span)
				
				if not arg.hasValue:
					assert arg.maybeValue
					symbol = state.lookupSymbol(arg.path, inValuePosition=True)
					if symbol:
						mir = SymbolAccess.readSymbol(symbol, arg.span)
				else:
					mir = state.analyzeNode(arg, param.valueType)
				
				state.appendDropPoint()
				block = state.block
				state.endScope()
				
				staticValue = None
				if mir == None or mir.type == None:
					assert state.failed
				elif param and not typesMatch(param.valueType, mir.type):
					logError(state, arg.span, 'expected type {}, found {}'.format(param.valueType, mir.type))
				else:
					staticValue = state.staticEval(mir.symbol, [block])
					if staticValue == None:
						logError(state, arg.span, 'expression cannot be statically evaluated')
				
				symbol = GenericAssocConst(param, staticValue, arg.span)
			else:
				t = state.resolveTypeRef(arg)
				symbol = GenericAssocType(param, t, arg.span)
				symbol.mangledName = t.mangledName
			
			if genericSymbolTable and param:
				paramSymbol = genericSymbolTable[param.name.content]
				self.argSymbolTable[param.name.content] = symbol
				self.argInfo.append(GenericArg(paramSymbol, symbol))
				self.mangledArgs += symbol.mangledName
	
	def constructType(self, state):
		from ..symbol.paramtype import ParamTypeSymbol, ParamTypeInst
		
		builtinName = self.path[0].content if len(self.path) == 1 else None
		if builtinName in BUILTIN_TYPES:
			logError(state, self.span, 'type `{}` is not a parameterized type'.format(builtinName))
			return None
		
		genericSymbol = state.lookupSymbol(self.path, inTypePosition=True)
		if genericSymbol:
			if type(genericSymbol) == ParamTypeInst:
				genericSymbol = genericSymbol.paramType
			
			if type(genericSymbol) != ParamTypeSymbol:
				name = '::'.join(self.path)
				logError(state, self.span, 'type `{}` is not a parameterized type'.format(name))
			
			if not genericSymbol.isGeneric:
				genericSymbol = None
		
		self.argInfo.append(GenericArg(genericSymbol.genericStruct, None))
		self.buildArgs(state, genericSymbol, genericSymbol.genericStruct.symbolTable if genericSymbol else None)
		
		if genericSymbol:
			structSymbol = Struct(genericSymbol.ast)
			structSymbol.paramType = genericSymbol
			structSymbol.type.symbolTable = self.argSymbolTable
			structSymbol.type.symbolTable[genericSymbol.name] = structSymbol
			structSymbol.analyze(state.ssstate, Deps(genericSymbol.ast))
			structSymbol.mangledName += '?' + self.mangledArgs + '$'
			
			self.argInfo[0].argSymbol = structSymbol
			
			for symbol in genericSymbol.type.symbolTable.values():
				if symbol.name in self.argSymbolTable:
					continue
				assert type(symbol) == Fn
				fnSymbolTable = dict(self.argSymbolTable)
				fnSymbolTable[genericSymbol.name] = ParamTypeInst(genericSymbol, structSymbol.type)
				self.argSymbolTable[symbol.name] = FnInst.getFnInst(state, symbol, fnSymbolTable, self.argInfo)
			
			return structSymbol
	
	def constructFn(self, state):
		genericSymbol = state.lookupSymbol(self.path, inValuePosition=True)
		if genericSymbol:
			if not (genericSymbol.isFn and genericSymbol.isGeneric):
				name = '::'.join(self.path)
				logError(state, self.path[-1].span, '`{}` is not a parameterized function'.format(name))
			
			if not genericSymbol.isGeneric:
				genericSymbol = None
		
		self.buildArgs(state, genericSymbol, genericSymbol.genericSymbolTable if genericSymbol else None)
		
		if genericSymbol:
			inst = FnInst.getFnInst(state, genericSymbol, self.argSymbolTable, self.argInfo)

class FnInst(ValueSymbol):
	def __init__(self, fn, mangledName, genericInc, symbolTable, instType=None):
		super().__init__(fn.ast.name, fn.ast.nameSpan, fn.ast.span, fn.ast.pub)
		self.mangledName = mangledName
		self.genericInc = genericInc
		
		self.fn = fn
		self.symbolTable = symbolTable
		self.type = instType if instType else fn.type.resolveGenerics(symbolTable)
		assert not self.type.isGenericType
		self.params = fn.params
		# self.isDropFnForType = None
		self.extern = False
		self.isFn = True
		self.unsafe = fn.unsafe
		self.cVarArgs = fn.cVarArgs
		self.cfg = fn.cfg
		self.inline = False
		self.genericReq = None
		self.isGeneric = False
		self.ir = None
	
	@staticmethod
	def getFnInst(state, fn, symbolTable, argInfo):
		if not fn.isGeneric:
			return fn
		
		mangledName = fn.mangledName + '?'
		genericInc = {}
		genericReq = set(fn.genericReq)
		for info in argInfo:
			if info.paramSymbol in genericReq:
				genericReq.remove(info.paramSymbol)
				genericInc[info.paramSymbol] = info.argSymbol
				mangledName += info.argSymbol.mangledName
		mangledName += '$'
		
		assert len(genericReq) == 0
		if mangledName in state.mod.fnInsts:
			existingInst = state.mod.fnInsts[mangledName]
			instType = fn.type.resolveGenerics(symbolTable)
			if typesMatch(instType, existingInst.type):
				return existingInst
			else:
				newInst = FnInst(fn, mangledName, genericInc, symbolTable, instType)
				newInst.extern = True
				return newInst
		
		newInst = FnInst(fn, mangledName, genericInc, symbolTable)
		state.mod.fnInsts[mangledName] = newInst
		return newInst
		
	def writeIR(self, state):
		assert 0