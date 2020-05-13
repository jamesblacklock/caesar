from .ast   import TypeSymbol, ValueSymbol
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
		
		fieldNames = []
		types = []
		for field in decl.fields:
			if field.name in fieldNames:
				logError(state, field.nameTok.span, 'duplicate field declared in {}'.format(decl.declType))
			else:
				fieldNames.append(field.name)
			
			fieldType = state.resolveTypeRef(field.typeRef)
			types.append(fieldType)
		
		layout = state.generateFieldLayout(types, fieldNames, isUnion=decl.isUnion)
		decl.type = StructType(decl.name, decl.isUnion, decl.dropFn, layout.align, layout.byteSize, layout.fields)
	
	def pretty(self, output, indent=0):
		output.write(self.declType + ' ', indent)
		output.write(self.name)
		output.write('\n')
		for field in self.fields[:-1]:
			field.pretty(output, indent + 1)
			output.write('\n')
		if len(self.fields) > 1:
			self.fields[-1].pretty(output, indent + 1)

class FieldDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, span):
		super().__init__(nameTok, typeRef, span)
		self.align = None
		self.offset = None
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.typeRef.pretty(output)
