from .symbol import Symbol, SymbolType
from ..types import Type

class Tuple(Symbol):
	def __init__(self, ast):
		super().__init__(SymbolType.TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.analyzed = False
		self.type = TupleType(self.name, self.span, self)
		self.types = None
	
	@property
	def symbolTable(self):
		return self.type.symbolTable
	
	def checkSig(self, state):
		self.types = [t.resolveSig(state) for t in self.ast.typeRefs]
		self.types = [t for t in self.types if t]
	
	def analyze(self, state, deps):
		if self in deps:
			s = 0
		
		if self.types == None:
			self.checkSig(state)
		
		for t in self.types:
			state.finishResolvingType(t, deps)
		
		layout = state.generateFieldLayout(self.types)
		self.type.applyLayout(layout)

class TupleType(Type):
	def __init__(self, name, span, symbol):
		super().__init__(name, span, symbol, isTupleType=True, isCompositeType=True)
		self.fields = None
		self.fieldDict = None
		self.anon = not self.name
		self.isDefinite = not self.anon
	
	@staticmethod
	def generateAnonTupleType(layout, span=None):
		tupleType = TupleType(None, span, None)
		tupleType.applyLayout(layout)
		return tupleType
	
	def applyLayout(self, layout):
		self.byteSize = layout.byteSize
		self.align = layout.align
		self.fields = layout.fields
		self.fieldDict = {field.name: field for field in self.fields}
		if self.anon:
			self.name = '({})'.format(', '.join(f.type.name for f in self.fields))
