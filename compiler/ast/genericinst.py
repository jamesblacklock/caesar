from symtable import SymbolTable
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
	def __init__(self, parent, param, t, span):
		super().__init__(SymbolType.TYPE, param.name.content, param.name.span, span, True)
		self.ast = param
		self.type = t
		self.mangledName = '{}.T{}'.format(parent.mangledName if parent else '_', self.name)
	
	def analyze(self, state, deps):
		pass

class GenericType(Type):
	def __init__(self, name, span, symbol):
		super().__init__(name, span, symbol, isGenericType=True)
		self.mangledName = symbol.mangledName
	
	def resolveGenerics(self, genericInc):
		if self.symbol in genericInc:
			return genericInc[self.symbol].type
		return self
	
	def refGenericType(self, state):
		state.genericReq.add(self.symbol)

class GenericArg:
	def __init__(self, paramSymbol, argSymbol):
		self.paramSymbol = paramSymbol
		self.argSymbol = argSymbol

class GenericConstructor:
	def __init__(self, argInfo, argSymbolTable, mangledArgs, incomplete, failed):
		self.mangledArgs = mangledArgs
		self.argInfo = argInfo
		self.argSymbolTable = argSymbolTable
		self.incomplete = incomplete
		self.failed = failed

class GenericInstValueRef(AST):
	def __init__(self, paramTypeRef, path, span):
		super().__init__(span, True)
		self.paramTypeRef = paramTypeRef
		self.path = path
		self.leakOwned = False
	
	def analyze(self, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, self, implicitType)

class GenericInst(AST):
	def __init__(self, path, args, span):
		super().__init__(span, True)
		self.path = path
		self.args = args
	
	def analyze(self, state, implicitType):
		inst = self.constructFn(state)
		return SymbolAccess.readSymbol(inst, self.span)
	
	def buildArgs(self, state, genericSymbol, genericSymbolTable, genericIncTable = None):
		genericParams = [None for _ in self.args]
		argInfo = []
		argSymbolTable = {}
		mangledArgs = ''
		incomplete = False
		failed = False
		
		if genericSymbol:
			if len(genericSymbol.genericParams) != len(self.args):
				name = '::'.join(self.path)
				logError(state, self.span, ('`{}` instatiated with wrong number of arguments (expected {}, found {})').format(
					name, len(genericSymbol.genericParams), len(self.args)))
			
			for (i, param) in enumerate(genericSymbol.genericParams):
				genericParams[i] = param
		
		for (param, arg) in zip(genericParams, self.args):
			symbol = None
			
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
						if symbol.isGeneric:
							incomplete = True
						else:
							mir = SymbolAccess.readSymbol(symbol, arg.span)
				else:
					mir = state.analyzeNode(arg, param.valueType)
				
				state.appendDropPoint()
				block = state.block
				state.endScope()
				
				staticValue = None
				if mir == None or mir.type == None:
					assert state.failed or symbol.isGeneric
				elif param and not typesMatch(param.valueType, mir.type):
					logError(state, arg.span, 'expected type {}, found {}'.format(param.valueType, mir.type))
				else:
					staticValue = state.staticEval(mir.symbol, [block])
					if staticValue == None:
						logError(state, arg.span, 'expression cannot be statically evaluated')
				
				if param and not (symbol and symbol.isGeneric):
					symbol = GenericAssocConst(param, staticValue, arg.span)
			else:
				t = None
				if genericIncTable and arg.name in genericIncTable:
					t = genericIncTable[arg.name]
				else:
					t = state.resolveTypeRef(arg)
				
				if param and t:
					# if t.isGenericType:
					# 	symbol = genericSymbolTable[param.name.content]
					# 	incomplete = True
					# else:
					symbol = GenericAssocType(genericSymbol, param, t, arg.span)
					symbol.mangledName = t.mangledName
			
			if symbol == None:
				assert state.failed
				failed = True
			elif genericSymbolTable and param:
				paramSymbol = genericSymbolTable[param.name.content]
				argSymbolTable[param.name.content] = symbol
				argInfo.append(GenericArg(paramSymbol, symbol))
				mangledArgs += symbol.mangledName
		
		return GenericConstructor(argInfo, argSymbolTable, mangledArgs, incomplete, failed)
	
	def constructType(self, state, genericIncTable = None):
		from ..symbol.paramtype import ParamTypeSymbol, ParamTypeInst
		
		builtinName = self.path[0].content if len(self.path) == 1 else None
		if builtinName in BUILTIN_TYPES:
			logError(state, self.span, 'type `{}` is not a parameterized type'.format(builtinName))
			return None
		
		mod = state.mod
		
		genericSymbol = state.lookupSymbol(self.path, inTypePosition=True)
		if genericSymbol:
			if type(genericSymbol) == ParamTypeInst:
				genericSymbol = genericSymbol.paramType
			
			if type(genericSymbol) != ParamTypeSymbol:
				name = '::'.join(self.path)
				logError(state, self.span, 'type `{}` is not a parameterized type'.format(name))
			
			if not genericSymbol.isGeneric:
				genericSymbol = None
		
		symbolTable = genericSymbol.genericStruct.symbolTable if genericSymbol else {}
		constr = self.buildArgs(state, genericSymbol, symbolTable, genericIncTable)
		
		if genericSymbol and not constr.failed:
			mangledName = genericSymbol.genericStruct.mangledName + '?' + constr.mangledArgs + '$'
			if mangledName in state.ssstate.typeInsts:
				return state.ssstate.typeInsts[mangledName]
			
			state.mod = genericSymbol.mod
			state.ssstate.mod = state.mod
			
			structSymbol = Struct(genericSymbol.ast)
			structSymbol.paramType = genericSymbol
			structSymbol.type.symbolTable = constr.argSymbolTable
			structSymbol.type.symbolTable[genericSymbol.name] = structSymbol
			structSymbol.analyze(state.ssstate, Deps(genericSymbol.ast))
			structSymbol.mangledName = mangledName
			structSymbol.genericInst = self
			state.ssstate.typeInsts[mangledName] = structSymbol
			
			constr.argInfo.insert(0, GenericArg(genericSymbol.genericStruct, ParamTypeInst(genericSymbol, structSymbol.type)))#structSymbol))
			
			for symbol in genericSymbol.type.symbolTable.values():
				if symbol.name in constr.argSymbolTable:
					continue
				assert type(symbol) == Fn
				fnSymbolTable = dict(constr.argSymbolTable)
				fnSymbolTable[genericSymbol.name] = ParamTypeInst(genericSymbol, structSymbol.type)
				fnInst = FnInst.getFnInst(state, symbol, fnSymbolTable, constr.argInfo)
				fnInst.genericInc[genericSymbol.genericStruct] = structSymbol
				constr.argSymbolTable[symbol.name] = fnInst
		else:
			structSymbol = None
		
		state.mod = mod
		state.ssstate.mod = state.mod
		return structSymbol
	
	def constructFn(self, state):
		mod = state.mod
		genericSymbol = state.lookupSymbol(self.path, inValuePosition=True)
		if genericSymbol:
			if not (genericSymbol.isFn and genericSymbol.isGeneric):
				name = '::'.join(self.path)
				logError(state, self.path[-1].span, '`{}` is not a parameterized function'.format(name))
			
			if not genericSymbol.isGeneric:
				genericSymbol = None
		
		constr = self.buildArgs(state, genericSymbol, genericSymbol.genericSymbolTable if genericSymbol else None)
		
		assert not constr.incomplete
		if genericSymbol and not constr.failed:
			state.mod = genericSymbol.mod
			state.ssstate.mod = state.mod
			inst = FnInst.getFnInst(state, genericSymbol, constr.argSymbolTable, constr.argInfo)
		else:
			inst = None
		
		state.mod = mod
		state.ssstate.mod = state.mod
		return inst
class FnInst(ValueSymbol):
	def __init__(self, state, fn, mangledName, genericInc, symbolTable, instType=None):
		super().__init__(fn.ast.name, fn.ast.nameSpan, fn.ast.span, fn.ast.pub)
		self.mangledName = mangledName
		self.genericInc = genericInc
		
		self.fn = fn
		self.symbolTable = symbolTable
		self.type = instType if instType else fn.type.resolveGenerics(genericInc)
		# assert not self.type.isGenericType
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

		# HACKY!
		self.contracts = set()
	
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
		
		genericIncTable = { k.name: v for (k, v) in genericInc.items() }
		for t in genericReq:
			assert t.genericInst
			u = t.genericInst.constructType(state, genericIncTable)
			genericInc[t] = u

		# assert len(genericReq) == 0
		if mangledName in state.ssstate.fnInsts:
			existingInst = state.ssstate.fnInsts[mangledName]
			instType = fn.type.resolveGenerics(genericInc)
			if typesMatch(instType, existingInst.type):
				return existingInst
			else:
				newInst = FnInst(state, fn, mangledName, genericInc, symbolTable, instType)
				newInst.extern = True
				return newInst
		
		newInst = FnInst(state, fn, mangledName, genericInc, symbolTable)
		state.ssstate.fnInsts[mangledName] = newInst
		return newInst