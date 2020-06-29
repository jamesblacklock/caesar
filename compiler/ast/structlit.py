from .ast               import AST
from ..not_done.typeref import NamedTypeRef
from ..not_done.structdecl import StructDecl
from ..not_done         import enumdecl
from ..ast              import primitive
from ..log              import logError
from ..mir.createstruct import CreateStruct, FieldInit
from ..mir.primitive    import IntValue

class FieldLit(AST):
	def __init__(self, nameTok, expr, span, name=None):
		super().__init__(span)
		self.nameTok = nameTok
		self.name = nameTok.content if nameTok else name
		self.expr = expr
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.expr.pretty(output)

class UnionLitFieldInfo:
	def __init__(self):
		self.noOffset = True
		self.unionField = True

class StructLit(AST):
	def __init__(self, typeRef, isUnion, fields, span):
		super().__init__(span, True)
		self.typeRef = typeRef
		self.isUnion = isUnion
		self.anon = typeRef == None
		self.nameTok = typeRef.path[-1] if typeRef else None
		self.name = self.nameTok.content if self.nameTok else None
		self.fields = fields

	def analyze(self, state, implicitType):
		fieldDict =  None
		uninitFields = set()
		resolvedType = None
		enumType = None
		variant = None
		# if self.type:
		# 	resolvedType = self.type
		# 	if resolvedType.isEnumType:
		# 		resolvedType = resolvedType.structType
		# el
		if self.typeRef:
			resolvedType = None
			# if type(self.typeRef) == NamedTypeRef:
			variants = implicitType.symbolTable if implicitType and implicitType.isEnumType else None
			symbol = state.lookupSymbol(self.typeRef.path, variants, inTypePosition=True)
			if type(symbol) == enumdecl.VariantDecl:
				variant = symbol
				enumType = variant.enumType
				resolvedType = variant.type
			else:
				resolvedType = state.resolveTypeRef(self.typeRef)
				if resolvedType.isUnknown:
					resolvedType = None
		else:
			resolvedType = implicitType
		
		if resolvedType:
			if resolvedType.isStructType:
				fieldDict = resolvedType.fieldDict
				uninitFields = { f for f in resolvedType.fields }
			else:
				logError(state, self.nameTok.span if self.nameTok else self.span, 
					'type `{}` is not a struct type'.format(resolvedType.name))
				resolvedType = None
		
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
						logError(state, fieldInit.nameTok.span, 
							'field `{}` was already initialized'.format(fieldInit.name))
						logExplain(state, initFields[fieldSymbol].nameTok.span, 
							'`{}` was initialized here'.format(fieldInit.name))
					else:
						initFields[fieldSymbol] = fieldInit
						uninitFields.remove(fieldSymbol)
				else:
					logError(state, fieldInit.nameTok.span, 
						'type `{}` has no field `{}`'.format(resolvedType.name, fieldInit.name))
			
			access = state.analyzeNode(fieldInit.expr, fieldType)
			if access:
				access = state.typeCheck(access, fieldType)
				accesses[fieldInit.name] = access
			else:
				fieldFailed = True
		
		if fieldFailed:
			return None
		
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
			resolvedType = StructDecl.generateAnonStructDecl(layout)
		
		inits = []
		for field in resolvedType.fields:
			if field.name in accesses:
				inits.append(FieldInit(accesses[field.name], field.offset))
		
		structValue = CreateStruct(inits, resolvedType, self.span)
		if enumType:
			structValue = CreateStruct.create(state, enumType, self.span, [
				CreateStruct.initStruct('$data', [
					CreateStruct.init('$' + variant.name, structValue)]),
				CreateStruct.init('$tag', IntValue(variant.tag.data, enumType.tagType, self.span))
			])
		
		return structValue
