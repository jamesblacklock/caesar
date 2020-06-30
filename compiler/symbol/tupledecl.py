from ..types   import TypeSymbol

class TupleDecl(TypeSymbol):
	def __init__(self, name, doccomment, typeRefs, pub, span):
		super().__init__(name, span, doccomment, isTupleType=True, isCompositeType=True)
		self.typeRefs = typeRefs
		self.pub = pub
		self.fields = None
		self.fieldDict = None
		self.anon = not self.name
		if self.anon:
			if not typeRefs:
				self.name = '<anonymous tuple>'
			else:
				self.name = '({})'.format(', '.join(typeRef.name for typeRef in typeRefs))
			self.isDefinite = False
	
	@staticmethod
	def generateAnonTupleDecl(layout):
		decl = TupleDecl(None, None, None, False, None)
		decl.applyLayout(layout)
		return decl
	
	def applyLayout(self, layout):
		self.byteSize = layout.byteSize
		self.align = layout.align
		self.fields = layout.fields
		self.fieldDict = {field.name: field for field in self.fields}
		if self.anon:
			self.name = '({})'.format(', '.join(f.type.name for f in self.fields))
	
	def analyzeSig(self, state):
		types = []
		for typeRef in self.typeRefs:
			fieldType = state.resolveTypeRefSig(typeRef)
			types.append(fieldType)
		
		layout = state.generateFieldLayout(types)
		self.applyLayout(layout)
