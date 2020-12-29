from enum import Enum

class SymbolType(Enum):
	MOD = 'MOD'
	VALUE = 'VALUE'
	# FN = 'FN'
	TYPE = 'TYPE'
	PARAM_TYPE = 'PARAM_TYPE'
	VARIANT = 'VARIANT'

class Symbol:
	def __init__(self, symbolType, name, nameSpan, span, pub=False, type=None):
		self.name = name
		self.nameSpan = nameSpan
		self.span = span
		self.symbolType = symbolType
		self.pub = pub
		self.type = type
		self.isImport = False
		self.unused = True
		self.isGeneric = False

class ValueSymbol(Symbol):
	def __init__(self, name, nameSpan, span, pub=False, type=None):
		super().__init__(SymbolType.VALUE, name, nameSpan, span, pub, type)
		self.isLocal = False
		self.isParam = False
		self.isStatic = False
		self.isConst = False
		self.isFn = False

class Deps:
	def __init__(self, symbol):
		self.chain = [symbol]
		self.all = {symbol}
	
	def push(self, symbol):
		self.chain.append(symbol)
		self.all.add(symbol)
	
	def pop(self):
		symbol = self.chain.pop()
		self.all.remove(symbol)
	
	def __contains__(self, symbol):
		return symbol in self.all