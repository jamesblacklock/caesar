from .ast           import SymbolAST
from ..symbol.tuple import Tuple

class TupleDecl(SymbolAST):
	def __init__(self, name, doccomment, typeRefs, pub, span):
		super().__init__(name, span, doccomment)
		self.typeRefs = typeRefs
		self.pub = pub
		self.anon = not self.name
	
	def createSymbol(self, state):
		return Tuple(self)
	
	def resolveSig(self, state, flow=None):
		symbol = self.createSymbol(state)
		return symbol.type
