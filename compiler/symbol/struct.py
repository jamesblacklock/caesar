from .symbol          import Symbol, SymbolType
from ..types          import Type

class Struct(Symbol):
	def __init__(self, ast):
		super().__init__(SymbolType.TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.analyzed = False
		self.type = StructType(self.name, self.span, self)
		self.types = None
		self.fieldNames = None
		self.fieldDecls = None
	
	@property
	def symbolTable(self):
		return self.type.symbolTable
	
	def checkSig(self, state):
		self.fieldDecls = self.ast.flattenedFieldDecls()
		declaredFields = {}
		fieldNames = []
		types = []
		for field in self.fieldDecls:
			if field.name in declaredFields:
				logError(state, field.nameSpan, 'duplicate field name')
				logExplain(state, declaredFields[field.name].nameSpan, 
					'field `{}` was previously declared here'.format(field.name))
			else:
				declaredFields[field.name] = field.nameSpan
				fieldType = state.resolveTypeRefSig(field.typeRef)
				fieldNames.append(field.name)
				types.append(fieldType)
		
		self.fieldNames = fieldNames
		self.types = types
	
	def analyze(self, state, deps):
		if self in deps:
			s = 0
		
		if self.types == None:
			self.checkSig(state)
		
		for fieldType in self.types:
			state.finishResolvingType(fieldType)
		
		layout = state.generateFieldLayout(self.types, self.fieldNames, self.fieldDecls)
		self.type.applyLayout(layout)
	
	# def pretty(self, output, indent=0):
	# 	output.write('union ' if self.isUnion else 'struct ', indent)
	# 	if self.name: output.write(self.name)
	# 	output.write('\n')
	# 	for field in self.fieldDecls[:-1]:
	# 		field.pretty(output, indent + 1)
	# 		output.write('\n')
	# 	if len(self.fieldDecls) > 0:
	# 		self.fieldDecls[-1].pretty(output, indent + 1)

class StructType(Type):
	def __init__(self, name, span, symbol):
		super().__init__(name, span, symbol, isStructType=True, isCompositeType=True)
		self.fields = None
		self.fieldDict = None
		self.anon = not self.name
		self.isDefinite = not self.anon
	
	@staticmethod
	def generateAnonStructType(layout, span=None):
		structType = StructType(None, span, None)
		structType.applyLayout(layout)
		return structType
	
	def applyLayout(self, layout):
		self.byteSize = layout.byteSize
		self.align = layout.align
		self.fields = layout.fields
		self.fieldDict = {field.name: field for field in self.fields}
		if self.anon:
			self.name = '{{{}}}'.format(', '.join('{}: {}'.format(f.name, f.type.name) for f in self.fields))
