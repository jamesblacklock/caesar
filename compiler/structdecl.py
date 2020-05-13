from .ast   import AST, TypeSymbol, ValueSymbol
from .types import StructType

class StructDecl(TypeSymbol):
	def __init__(self, nameTok, isUnion, doccomment, fields, span):
		super().__init__(nameTok, span, doccomment)
		self.fields = fields
		self.isUnion = isUnion
		self.declType = 'union' if isUnion else 'struct'
	
	def analyzeSig(decl, state):
		from . import attrs
		attrs.invokeAttrs(state, decl)
		
		fields = []
		for (i, fieldDecl) in enumerate(decl.fields):
			if type(fieldDecl) == UnionFields:
				for (j, unionField) in enumerate(fieldDecl.fields):
					unionField.unionField = True
					if j+1 < len(fieldDecl.fields):
						unionField.noOffset = True
					fields.append(unionField)
			else:
				if decl.isUnion:
					fieldDecl.unionField = True
					if i+1 < len(decl.fields):
						fieldDecl.noOffset = True
				fields.append(fieldDecl)
		decl.fields = fields
		
		fieldNames = []
		types = []
		for field in decl.fields:
			if field.name in fieldNames:
				logError(state, field.nameTok.span, 'duplicate field declared in {}'.format(decl.declType))
			
			fieldType = state.resolveTypeRef(field.typeRef)
			fieldNames.append(field.name)
			types.append(fieldType)
		
		layout = state.generateFieldLayout(types, fieldNames, decl.fields)
		decl.type = StructType(decl.name, decl.dropFn, layout.align, layout.byteSize, layout.fields)
	
	def pretty(self, output, indent=0):
		output.write(self.declType + ' ', indent)
		output.write(self.name)
		output.write('\n')
		for field in self.fields[:-1]:
			field.pretty(output, indent + 1)
			output.write('\n')
		if len(self.fields) > 1:
			self.fields[-1].pretty(output, indent + 1)

class UnionFields(AST):
	def __init__(self, fields, span):
		super().__init__(span)
		self.fields = fields

class FieldDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, span):
		super().__init__(nameTok, typeRef, span)
		self.align = None
		self.offset = None
		self.unionField = False
		self.noOffset = False
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.typeRef.pretty(output)
