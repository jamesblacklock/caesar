from .symbol import Symbol, SymbolType
from ..types import Type
from .mod    import PatchMod

class Struct(Symbol):
	def __init__(self, ast):
		super().__init__(SymbolType.TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.analyzed = False
		self.type = StructType(self.name, self.span, self)
		self.types = None
		self.fieldNames = None
		self.fieldDecls = None
		self.mod = None
		self.hasPrivateFields = False
		self.hasReadOnlyFields = False
		self.paramType = None
	
	@property
	def symbolTable(self):
		return self.type.symbolTable
	
	def checkSig(self, state):
		self.mod = PatchMod(state.mod, self.symbolTable)
		state.mod = self.mod
		
		self.fieldDecls = self.ast.flattenedFieldDecls()
		declaredFields = {}
		fieldNames = []
		types = []
		for field in self.fieldDecls:
			self.hasPrivateFields = self.hasPrivateFields or not field.pub
			self.hasReadOnlyFields = self.hasReadOnlyFields or not field.mut
			if field.name in declaredFields:
				logError(state, field.nameSpan, 'duplicate field name')
				logExplain(state, declaredFields[field.name].nameSpan, 
					'field `{}` was previously declared here'.format(field.name))
				continue
			
			declaredFields[field.name] = field.nameSpan
			fieldType = field.typeRef.resolveSig(state)
			if fieldType:
				fieldNames.append(field.name)
				types.append(fieldType)
		
		self.fieldNames = fieldNames
		self.types = types
		
		state.mod = state.mod.parent
	
	def analyze(self, state, deps):
		if self.analyzed:
			return
		self.analyzed = True
		
		if self in deps:
			s = 0
		
		if self.types == None:
			self.checkSig(state)
		
		for fieldType in self.types:
			state.finishResolvingType(fieldType)
		
		layout = state.generateFieldLayout(self.types, self.fieldNames, self.fieldDecls)
		self.type.applyLayout(layout)

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
	
	def resolveGenerics(self, symbolTable):
		if self.name in symbolTable:
			return symbolTable[self.name].type
