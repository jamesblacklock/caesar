from .symbol           import Symbol, SymbolType
from ..ast.genericinst import GenericAssocConst, GenericAssocType, GenericType
from .struct           import Struct
from ..log             import logError, logExplain

class ParamTypeInst(Symbol):
	def __init__(self, paramType, defaultType):
		super().__init__(SymbolType.TYPE, paramType.name, paramType.nameSpan, paramType.span)
		self.paramType = paramType
		self.type = defaultType

class ParamTypeSymbol(Symbol):
	def __init__(self, ast, genericParams):
		super().__init__(SymbolType.PARAM_TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.genericParams = genericParams
		self.mod = None
		self.genericStruct = Struct(self.ast)
		self.genericStruct.type.isGenericType = True
		self.type = self.genericStruct.type
		self.analyzed = False
		self.sigWasChecked = False
		self.isGeneric = True
	
	@property
	def symbolTable(self):
		return self.type.symbolTable
	
	def checkSig(self, state):
		self.genericStruct.symbolTable[self.name] = ParamTypeInst(self, self.genericStruct.type)
		
		paramNames = {}
		for param in self.genericParams:
			if param.name.content in paramNames:
				logError(state, param.span, 'duplicate parameter name')
				logExplain(state, paramNames[param.name.content], '`{}` was previously declared here'.format(param.name.content))
				continue
			
			if param.valueType:
				symbol = GenericAssocConst(param, None, param.span)
			else:
				symbol = GenericAssocType(param, None, param.span)
				symbol.type = GenericType(param.name.content, param.name.span, symbol)
			
			self.genericStruct.symbolTable[param.name.content] = symbol
			paramNames[param.name.content] = param.span
		
		self.genericStruct.checkSig(state)
		self.mangledName = state.mangleName(self)
		self.sigWasChecked = True
	
	def analyze(self, state, deps):
		if self.analyzed:
			return
		self.analyzed = True
		
		if self.sigWasChecked == False:
			self.checkSig(state)
		
		self.genericStruct.analyze(state, deps)
