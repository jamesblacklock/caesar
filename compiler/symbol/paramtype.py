from .symbol      import Symbol, ValueSymbol, SymbolType, Deps
from .fn          import Fn
from .struct      import Struct
from ..types      import typesMatch
from ..log        import logError, logExplain
from ..mir.flow   import CFGBuilder
from ..mir.access import SymbolAccess

class ParamTypeAssocConst(ValueSymbol):
	def __init__(self, param, staticValue, span):
		super().__init__(param.name.content, param.name.span, span, True)
		self.ast = param
		self.staticValue = staticValue
		self.isConst = True
		self.type = param.valueType
		self.contracts = {}

class ParamTypeAssocType(Symbol):
	def __init__(self, param, type, span):
		super().__init__(SymbolType.TYPE, param.name.content, param.name.span, span, True)
		self.ast = param
		self.type = type

class ParamTypeMod:
	def __init__(self):
		self.symbolTable = {}
		self.parent = None
		self.transparent = True

class ParamTypeSymbol(Symbol):
	def __init__(self, ast, typeParams):
		super().__init__(SymbolType.PARAM_TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.typeParams = typeParams
		self.mod = None
		self.genericStruct = Struct(self.ast)
		self.type = self.genericStruct.type
		self.analyzed = False
	
	@property
	def symbolTable(self):
		return self.type.symbolTable
	
	def checkSig(self, state):
		paramNames = {}
		for param in self.typeParams:
			if param.name.content in paramNames:
				logError(state, param.span, 'duplicate parameter name')
				logExplain(state, paramNames[param.name.content], '`{}` was previously declared here'.format(param.name.content))
				continue
			
			if param.valueType:
				symbol = ParamTypeAssocConst(param, None, param.span)
			else:
				symbol = ParamTypeAssocType(param, None, param.span)
			
			self.genericStruct.symbolTable[param.name.content] = symbol
			paramNames[param.name.content] = param.span
		
		self.genericStruct.checkSig(state)
	
	def analyze(self, state, deps):
		if self.analyzed:
			return
		self.analyzed = True
		
		if self.mod == None:
			self.checkSig(state)
		
		self.genericStruct.analyze(state, deps)
	
	def constructFromArgs(self, state, args, span):
		if len(args) != len(self.typeParams):
			logError(state, span, 'type instantiated with wrong number of arguments (expected {}, found {})'
				.format(len(self.typeParams), len(args)))
			return None
		
		symbolTable = {}
		
		for (param, arg) in zip(self.typeParams, args):
			if param.valueType:
				if not (arg.hasValue or arg.maybeValue):
					logError(state, arg.span, 'found type reference where a value was expected')
					continue
				
				state.beginScope(self.span)
				
				if not arg.hasValue and arg.maybeValue:
					symbol = state.lookupSymbol(arg.path, inValuePosition=True)
					if symbol == None:
						continue
					mir = SymbolAccess.readSymbol(symbol, arg.span)
				else:
					mir = state.analyzeNode(arg, param.valueType)
				
				state.appendDropPoint()
				block = state.block
				state.endScope()
				
				if mir == None or mir.type == None:
					assert state.failed
					continue
				
				if not typesMatch(param.valueType, mir.type):
					logError(state, arg.span, 'expected type {}, found {}'.format(param.valueType, mir.type))
					continue
				
				staticValue = state.staticEval(mir.symbol, [block])
				if staticValue == None:
					logError(state, arg.span, 'expression cannot be statically evaluated')
					continue
				
				symbol = ParamTypeAssocConst(param, staticValue, arg.span)
			else:
				if arg.hasValue:
					logError(state, arg.span, 'found value expression where a type was expected')
					continue
				
				t = state.resolveTypeRef(arg)
				symbol = ParamTypeAssocType(param, t, arg.span)
		
			symbolTable[param.name.content] = symbol
		
		structSymbol = Struct(self.ast)
		structSymbol.paramType = self
		structSymbol.type.symbolTable = symbolTable
		structSymbol.analyze(state.ssstate, Deps(self.ast))
		
		for symbol in self.type.symbolTable.values():
			if symbol.name in symbolTable:
				continue
			assert type(symbol) == Fn
			symbolTable[symbol.name] = symbol
		
		return structSymbol.type
		
			