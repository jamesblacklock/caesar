from .ast import TypeSymbol

class AliasDecl(TypeSymbol):
	def __init__(self, nameTok, typeRef, span, doccomment):
		super().__init__(nameTok, span, doccomment)
		self.typeRef = typeRef
	
	def analyzeSig(self, state):
		self.type = state.resolveTypeRefSig(self.typeRef)
	
	def pretty(self, output, indent=0):
		output.write('alias ', indent)
		output.write(self.name)
		output.write(' = ')
		if self.type:
			output.write(self.type.name)
		else:
			output.write(self.typeRef.name)