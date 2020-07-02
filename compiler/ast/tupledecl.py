from .ast           import Symbol
from ..symbol.tuple import Tuple

class TupleDecl(Symbol):
	def __init__(self, name, doccomment, typeRefs, pub, span):
		super().__init__(name, span, doccomment)
		self.typeRefs = typeRefs
		self.pub = pub
		self.anon = not self.name
	
	def createSymbol(self, state):
		return Tuple(self)
