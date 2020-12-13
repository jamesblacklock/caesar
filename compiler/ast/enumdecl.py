from .ast          import AST, SymbolAST
from ..symbol.enum import Enum, Variant

class VariantDecl(AST):
	def __init__(self, name, typeRef, span):
		super().__init__(span)
		self.nameSpan = name.span
		self.name = name.content
		self.typeRef = typeRef
		self.enumType = None
		self.type = None
		self.tag = None
	
	def createSymbol(self, state):
		return Variant(self)

class EnumDecl(SymbolAST):
	def __init__(self, name, doccomment, variants, span):
		super().__init__(name, span, doccomment)
		self.variants = variants
	
	def createSymbol(self, state):
		variants = [v.createSymbol(state) for v in self.variants]
		return Enum(self, variants)
