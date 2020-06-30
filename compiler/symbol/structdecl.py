from ..ast.ast import AST, ValueSymbol
from ..types   import TypeSymbol

class StructDecl(TypeSymbol):
	def __init__(self, nameTok, isUnion, doccomment, fields, pub, span):
		super().__init__(nameTok, span, doccomment, isStructType=True, isCompositeType=True)
		self.fieldDecls = fields
		self.isUnion = isUnion
		self.pub = pub
		self.fields = None
		self.fieldDict = None
		self.anon = not self.name
		if self.anon:
			if not fields:
				self.name = '<anonymous struct>'
			else:
				self.name = '{{{}}}'.format(', '.join('{}: {}'.format(f.name, f.typeRef.name) for f in fields))
			self.isDefinite = False
	
	@staticmethod
	def generateAnonStructDecl(layout):
		decl = StructDecl(None, False, None, None, False, None)
		decl.applyLayout(layout)
		return decl
	
	def applyLayout(self, layout):
		self.byteSize = layout.byteSize
		self.align = layout.align
		self.fields = layout.fields
		self.fieldDict = {field.name: field for field in self.fields}
		if self.anon:
			self.name = '{{{}}}'.format(', '.join('{}: {}'.format(f.name, f.type.name) for f in self.fields))
	
	def analyzeSig(decl, state):
		fieldDecls = []
		for (i, fieldDecl) in enumerate(decl.fieldDecls):
			if type(fieldDecl) == UnionFields:
				for (j, unionField) in enumerate(fieldDecl.fieldDecls):
					unionField.unionField = True
					if j+1 < len(fieldDecl.fieldDecls):
						unionField.noOffset = True
					fieldDecls.append(unionField)
			else:
				if decl.isUnion:
					fieldDecl.unionField = True
					if i+1 < len(decl.fieldDecls):
						fieldDecl.noOffset = True
				fieldDecls.append(fieldDecl)
		decl.fieldDecls = fieldDecls
		
		fieldNames = []
		types = []
		for field in decl.fieldDecls:
			if field.name in fieldNames:
				logError(state, field.nameSpan, 
					'duplicate field declared in {}'.format('union' if isUnion else 'struct'))
			
			fieldType = state.resolveTypeRefSig(field.typeRef)
			fieldNames.append(field.name)
			types.append(fieldType)
		
		layout = state.generateFieldLayout(types, fieldNames, decl.fieldDecls)
		decl.applyLayout(layout)
	
	def pretty(self, output, indent=0):
		output.write('union ' if self.isUnion else 'struct ', indent)
		if self.name: output.write(self.name)
		output.write('\n')
		for field in self.fieldDecls[:-1]:
			field.pretty(output, indent + 1)
			output.write('\n')
		if len(self.fieldDecls) > 0:
			self.fieldDecls[-1].pretty(output, indent + 1)

class UnionFields(AST):
	def __init__(self, fields, span):
		super().__init__(span)
		self.fieldDecls = fields

class FieldDecl(AST):
	def __init__(self, nameTok, typeRef, pub, span):
		super().__init__(span)
		self.nameSpan = nameTok.span
		self.name = nameTok.content
		self.typeRef = typeRef
		self.align = None
		self.offset = None
		self.unionField = False
		self.noOffset = False
		self.pub = pub
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.typeRef.pretty(output)
