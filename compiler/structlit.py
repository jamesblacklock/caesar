from .ast        import StaticData, StaticDataType, AST, ValueExpr, TypeModifiers
from .typeref    import NamedTypeRef
from .types      import typesMatch
from .structdecl import StructDecl
from .           import enumdecl, primitive, ir
from .log        import logError

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

class StructLit(ValueExpr):
	def __init__(self, typeRef, isUnion, fields, span, typeSymbol=None):
		super().__init__(span)
		self.typeRef = typeRef
		self.isUnion = isUnion
		self.anon = typeRef == None
		self.nameTok = typeRef.path[-1] if typeRef else None
		self.name = self.nameTok.content if self.nameTok else None
		self.fields = fields
		self.typeModifiers = TypeModifiers(False)
		self.type = typeSymbol

	def analyze(expr, state, implicitType):
		fieldDict =  None
		uninitFields = set()
		resolvedType = None
		enumType = None
		variant = None
		if expr.type:
			resolvedType = expr.type
			if resolvedType.isEnumType:
				resolvedType = resolvedType.structType
		elif expr.typeRef:
			resolvedType = None
			if type(expr.typeRef) == NamedTypeRef:
				symbolTable = implicitType.symbolTable if implicitType and implicitType.isEnumType else None
				symbol = state.lookupSymbol(expr.typeRef.path, symbolTable, inTypePosition=True)
				if type(symbol) == enumdecl.VariantDecl:
					variant = symbol
					enumType = variant.enumType
					resolvedType = variant.type
			if resolvedType == None:
				resolvedType = state.resolveTypeRef(expr.typeRef)
			if resolvedType.isUnknown:
				expr.type = resolvedType
				return
		elif implicitType:
			resolvedType = implicitType
		
		if enumType:
			expr.typeRef = None
			expr.type = variant.type
			dataField = FieldLit(None, expr, expr.span, name='$' + variant.name)
			dataStruct = StructLit(None, True, [dataField], expr.span, typeSymbol=enumType.structType.fields[0].type)
			tagValue = primitive.IntLit(None, False, expr.span, value=variant.tag.data)
			fields = [
				FieldLit(None, dataStruct, expr.span, name='$data'), 
				FieldLit(None, tagValue, expr.span, name='$tag')
			]
			expr = StructLit(None, False, fields, expr.span, typeSymbol=enumType)
			return state.analyzeNode(expr)
		
		if resolvedType:
			if resolvedType.isStructType:
				fieldDict = resolvedType.fieldDict
				uninitFields = { f for f in resolvedType.fields }
			else:
				logError(state, expr.nameTok.span if expr.nameTok else expr.span, 
					'type `{}` is not a struct type'.format(resolvedType.name))
				return
		
		initFields = {}
		for fieldInit in expr.fields:
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
			
			fieldInit.expr = state.analyzeNode(fieldInit.expr, fieldType)
			fieldInit.expr = state.typeCheck(fieldInit.expr, fieldType)
		
		if resolvedType == None:
			fieldTypes = [field.expr.type for field in expr.fields]
			fieldNames = [field.name for field in expr.fields]
			fieldInfo = None
			if expr.isUnion:
				fieldInfo = [UnionLitFieldInfo() for _ in expr.fields]
			layout = state.generateFieldLayout(fieldTypes, fieldNames, fieldInfo)
			resolvedType = StructDecl.generateAnonStructDecl(layout)
		
		if expr.type == None:
			expr.type = resolvedType
	
	def staticEval(self, state):
		structBytes = [0 for _ in range(0, self.type.byteSize)]
		for fieldLit in self.fields:
			fieldInfo = self.type.fieldDict[fieldLit.name]
			staticFieldValue = fieldLit.expr.staticEval(state)
			if staticFieldValue == None:
				return None
			
			fieldBytes = staticFieldValue.toBytes()
			end = fieldInfo.offset + len(fieldBytes)
			structBytes[fieldInfo.offset : end] = fieldBytes
		
		fType = ir.FundamentalType.fromResolvedType(self.type)
		return StaticData(structBytes, StaticDataType.BYTES, fType)
	
	def accessSymbols(self, scope):
		for field in self.fields:
			field.expr.accessSymbols(scope)
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Res(ast, fType))
		state.initStructFields(ast, 0)
	
	def pretty(self, output, indent=0):
		if self.typeRef:
			self.typeRef.pretty(output, indent)
		output.write('\n')
		for field in self.fields[:-1]:
			field.pretty(output, indent + 1)
			output.write('\n')
		if len(self.fields) > 0:
			self.fields[-1].pretty(output, indent + 1)

class UnionLitFieldInfo:
	def __init__(self):
		self.noOffset = True
		self.unionField = True
