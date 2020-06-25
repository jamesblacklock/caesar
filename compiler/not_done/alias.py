from ..types import Symbol, TypeSymbol

class AliasDecl(Symbol):
	def __init__(self, nameTok, typeRef, span, doccomment):
		super().__init__(nameTok, span, doccomment)
		self.typeRef = typeRef
	
	def analyzeSig(self, state):
		self.symbol = state.resolveTypeRefSig(self.typeRef)
	
	def analyze(self, state, implicitType):
		self.symbol = state.finishResolvingType(self.type)
	
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
		super().__init__(nameTok, span, doccomment, isTypeDef=True)
		self.typeRef = typeRef
		self.baseType = None
	
	def analyzeSig(self, state):
		self.baseType = state.resolveTypeRefSig(self.typeRef)
		self.byteSize = self.baseType.byteSize
		self.align = self.baseType.align
	
	def pretty(self, output, indent=0):
		output.write('type ', indent)
		output.write(self.name)
		output.write(' = ')
		if self.baseType:
			output.write(self.baseType.name)
		else:
			output.write(self.typeRef.name)
