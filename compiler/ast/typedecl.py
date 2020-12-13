from .ast               import SymbolAST
from ..symbol.typealias import TypeAlias

class TypeDecl(SymbolAST):
	def __init__(self, name, typeRef, span, doccomment):
		super().__init__(name, span, doccomment)
		self.typeRef = typeRef
	
	def createSymbol(self, state):
		return TypeAlias(self)
	
	# def analyzeSig(self, state):
	# 	self.baseType = self.typeRef.resolveSig(state)
	# 	self.byteSize = self.baseType.byteSize
	# 	self.align = self.baseType.align
