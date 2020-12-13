from .symbol         import Symbol, SymbolType
from ..mir.mir       import StaticData, StaticDataType
from ..types         import Type, Void, UInt8, UInt16, U8_MAX, U16_MAX
from ..ast.structlit import UnionLitFieldInfo
from .struct         import StructType
from ..ir            import FundamentalType
from ..log           import logError

class Variant(Symbol):
	def __init__(self, ast):
		super().__init__(SymbolType.VARIANT, ast.name, ast.nameSpan, ast.span, True)
		self.ast = ast
	
	def checkSig(self, state):
		# should be checked by Enum.checkSig
		assert 0

class Enum(Symbol):
	def __init__(self, ast, variants):
		super().__init__(SymbolType.TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.type = EnumType(self.name, self.span, self, variants)
	
	@property
	def symbolTable(self):
		return self.type.symbolTable
	
	def checkSig(self, state):
		if len(self.type.variants) > U8_MAX:
			self.type.tagType = UInt16
			if len(self.type.variants) > U16_MAX:
				logError(state, self.nameSpan, 
					'Congratulations, you\'ve broken the compiler! (too many variants)')
		
		fTagType = FundamentalType.fromResolvedType(self.type.tagType)
		for (i, v) in enumerate(self.type.variants):
			assert v.name not in self.type.symbolTable
			self.type.symbolTable[v.name] = v
			v.enumType = self.type
			v.tag = StaticData(i, StaticDataType.INT, fTagType)
			if v.ast.typeRef:
				v.type = v.ast.typeRef.resolveSig(state)
	
	def analyze(self, state, deps):
		if self in deps:
			s = 0
		
		fieldTypes = []
		fieldNames = []
		fieldInfo = []
		for v in self.type.variants:
			if v.type:
				state.finishResolvingType(v.type)
				fieldTypes.append(v.type)
				fieldNames.append('$' + v.name)
				fieldInfo.append(UnionLitFieldInfo(False, False))
		
		if len(fieldTypes) > 0:
			layout = state.generateFieldLayout(fieldTypes, fieldNames, fieldInfo)
			self.type.dataType = StructType.generateAnonStructType(layout)
		
		layout = state.generateFieldLayout([self.type.dataType, self.type.tagType], ['$data', '$tag'])
		self.type.structType = StructType.generateAnonStructType(layout)
		
		self.type.byteSize = self.type.structType.byteSize
		self.type.align = self.type.structType.align
	
class EnumType(Type):
	def __init__(self, name, span, symbol, variants):
		super().__init__(name, span, symbol, isEnumType=True)
		self.variants = variants
		self.dataType = Void
		self.tagType = UInt8
		self.structType = None
