from .ast               import Symbol
from ..symbol.typealias import TypeAlias

class TypeDecl(Symbol):
	def __init__(self, name, typeRef, span, doccomment):
		super().__init__(name, span, doccomment)
		self.typeRef = typeRef
	
	def createSymbol(self, state):
		return TypeAlias(self)
	
	# def analyzeSig(self, state):
	# 	self.baseType = state.resolveTypeRefSig(self.typeRef)
	# 	self.byteSize = self.baseType.byteSize
	# 	self.align = self.baseType.align
	
	# def pretty(self, output, indent=0):
	# 	output.write('type ', indent)
	# 	output.write(self.name)
	# 	output.write(' = ')
	# 	if self.baseType:
	# 		output.write(self.baseType.name)
	# 	else:
	# 		output.write(self.typeRef.name)
