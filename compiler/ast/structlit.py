from .ast               import AST
from .typeref           import NamedTypeRef
from ..symbol.struct    import StructType
from ..symbol.symbol    import SymbolType
from ..mir.createstruct import CreateStruct, FieldInit
from ..mir.primitive    import IntValue
from ..log              import logError

class FieldLit(AST):
	def __init__(self, name, expr, span):
		super().__init__(span)
		self.nameSpan = name.span
		self.name = name.content
		self.expr = expr

class UnionLitFieldInfo:
	def __init__(self):
		self.noOffset = True
		self.unionField = True
		self.pub = True

class StructLit(AST):
	def __init__(self, typeRef, isUnion, fields, span):
		super().__init__(span, True)
		self.typeRef = typeRef
		self.isUnion = isUnion
		self.fields = fields

	def analyze(self, state, implicitType):
		enumType = None
		variant = None
		if self.typeRef:
			variants = implicitType.symbolTable if implicitType and implicitType.isEnumType else None
			symbol = state.lookupSymbol(self.typeRef.path, variants, inTypePosition=True)
			if symbol:
				if symbol.symbolType == SymbolType.VARIANT:
					variant = symbol
					enumType = variant.enumType
					implicitType = variant.type
				else:
					implicitType = symbol.type
					if implicitType.isUnknown:
						implicitType = None
				
				if implicitType and not implicitType.isStructType:
					span = self.path[-1].span if self.path else self.span
					logError(state, span, 'type `{}` is not a struct type'.format(implicitType.name))
					implicitType = None
		
		fieldDict =  None
		if implicitType:
			if implicitType.isStructType:
				fieldDict = implicitType.fieldDict
			else:
				implicitType = None
		
		fieldFailed = False
		initFields = {}
		accesses = {}
		for fieldInit in self.fields:
			fieldType = None
			if fieldDict:
				if fieldInit.name in fieldDict:
					fieldSymbol = fieldDict[fieldInit.name]
					fieldType = fieldSymbol.type
					if fieldSymbol in initFields:
						logError(state, fieldInit.nameSpan, 
							'field `{}` was already initialized'.format(fieldInit.name))
						logExplain(state, initFields[fieldSymbol].nameSpan, 
							'`{}` was initialized here'.format(fieldInit.name))
					else:
						initFields[fieldSymbol] = fieldInit
				else:
					logError(state, fieldInit.nameSpan, 
						'type `{}` has no field `{}`'.format(implicitType.name, fieldInit.name))
			
			access = state.analyzeNode(fieldInit.expr, fieldType)
			if access:
				access = state.typeCheck(access, fieldType)
				accesses[fieldInit.name] = access
			else:
				fieldFailed = True
		
		if fieldFailed:
			return None
		
		resolvedType = implicitType
		if resolvedType == None:
			fieldTypes = []
			fieldNames = []
			for field in self.fields:
				fieldTypes.append(accesses[field.name].type)
				fieldNames.append(field.name)
			
			fieldInfo = None
			if self.isUnion:
				fieldInfo = [UnionLitFieldInfo() for _ in self.fields]
			layout = state.generateFieldLayout(fieldTypes, fieldNames, fieldInfo)
			resolvedType = StructType.generateAnonStructType(layout)
		
		inits = []
		for field in resolvedType.fields:
			if field.name in accesses:
				inits.append(FieldInit(accesses[field.name], field))
		
		structValue = CreateStruct(inits, resolvedType, self.span)
		if enumType:
			structValue = CreateStruct.create(state, enumType, self.span, [
				CreateStruct.initStruct('$data', [
					CreateStruct.init('$' + variant.name, structValue)]),
				CreateStruct.init('$tag', IntValue(variant.tag.data, enumType.tagType, self.span))
			])
		
		return structValue
