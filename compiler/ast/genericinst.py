from .ast            import AST
from ..symbol.symbol import Symbol, ValueSymbol, SymbolType, Deps
from ..mir.access    import SymbolAccess
from ..mir.mir       import StaticDataType
from ..log           import logError
from ..types         import typesMatch, Type

class GenericAssocConst(ValueSymbol):
	def __init__(self, param, staticValue, span):
		super().__init__(param.name.content, param.name.span, span, True)
		self.ast = param
		self.staticValue = staticValue
		self.isConst = True
		self.isGeneric = True
		self.type = param.valueType
		self.contracts = {}
		if self.staticValue:
			assert self.staticValue.dataType == StaticDataType.INT
			self.mangledName = 'C{}i{}'.format(len(str(self.staticValue.data)) + 1, self.staticValue.data)

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

