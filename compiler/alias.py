from .ast   import TypeSymbol
from .types import TypeDefType

class AliasDecl(TypeSymbol):
	def __init__(self, nameTok, typeRef, span, doccomment):
		super().__init__(nameTok, span, doccomment)
		self.typeRef = typeRef
	
	def analyzeSig(self, state):
		self.type = state.resolveTypeRefSig(self.typeRef)
	
	def analyze(self, state, implicitType):
		self.type = state.finishResolvingType(self.type)
	
	def pretty(self, output, indent=0):
		output.write('alias ', indent)
		output.write(self.name)
		output.write(' = ')
		if self.type:
			output.write(self.type.name)
		else:
			output.write(self.typeRef.name)

class TypeDecl(TypeSymbol):
	def __init__(self, nameTok, typeRef, span, doccomment):
		super().__init__(nameTok, span, doccomment)
		self.typeRef = typeRef
	
	def analyzeSig(self, state):
		baseType = state.resolveTypeRefSig(self.typeRef)
		self.type = TypeDefType(self.name, baseType)
	
	def pretty(self, output, indent=0):
		output.write('type ', indent)
		output.write(self.name)
		output.write(' = ')
		if self.type:
			output.write(self.type.name)
		else:
			output.write(self.typeRef.name)
