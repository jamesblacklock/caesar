from ..ast.ast       import AST
from ..mir.mir       import StaticData, StaticDataType
from ..types         import Void, UInt8, UInt16, TypeSymbol, U8_MAX, U16_MAX
from ..ast.structlit import UnionLitFieldInfo
from .structdecl     import StructDecl
from ..ir            import I8, I16, FundamentalType
from ..log           import logError

class EnumDecl(TypeSymbol):
	def __init__(self, name, doccomment, variants, span):
		super().__init__(name, span, doccomment, isEnumType=True)
		self.variants = variants
		self.dataType = Void
		self.tagType = UInt8
		self.structType = None
	
	def analyzeSig(self, state):
		if len(self.variants) > U8_MAX:
			self.tagType = UInt16
			if len(self.variants) > U16_MAX:
				logError(state, self.nameSpan, 
					'Congratulations, you\'ve broken the compiler! (too many variants)')
		
		fTagType = FundamentalType.fromResolvedType(self.tagType)
		fieldTypes = []
		fieldNames = []
		fieldInfo = []
		for (i, v) in enumerate(self.variants):
			self.symbolTable[v.name] = v
			v.enumType = self
			v.tag = StaticData(i, StaticDataType.INT, fTagType)
			if v.typeRef:
				v.type = state.resolveTypeRefSig(v.typeRef)
				fieldTypes.append(v.type)
				fieldNames.append('$' + v.name)
				fieldInfo.append(UnionLitFieldInfo())
		
		if len(fieldTypes) > 0:
			layout = state.generateFieldLayout(fieldTypes, fieldNames, fieldInfo)
			self.dataType = StructDecl.generateAnonStructDecl(layout)
		
		layout = state.generateFieldLayout([self.dataType, self.tagType], ['$data', '$tag'])
		self.structType = StructDecl.generateAnonStructDecl(layout)
		
		self.byteSize = self.structType.byteSize
		self.align = self.structType.align
	
	# def pretty(self, output, indent=0):
	# 	output.write('enum ' + self.name, indent)
	# 	output.write('\n')
	# 	for variant in self.variants[:-1]:
	# 		variant.pretty(output, indent + 1)
	# 		output.write('\n')
	# 	if len(self.variants) > 0:
	# 		self.variants[-1].pretty(output, indent + 1)

class VariantDecl(AST):
	def __init__(self, name, typeRef, span):
		super().__init__(span)
		self.nameSpan = name.span
		self.name = name.content
		self.typeRef = typeRef
		self.enumType = None
		self.type = None
		self.tag = None
	
	# def pretty(self, output, indent=0):
	# 	output.write(self.name, indent)
	# 	if self.type:
	# 		self.type.pretty(output)
	# 	elif self.typeRef:
	# 		self.typeRef.pretty(output)
