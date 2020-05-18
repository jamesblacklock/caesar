from .ast   import StaticData, StaticDataType, AST, ValueExpr, TypeModifiers
from .types import typesMatch, StructType
from .log   import logError
from .      import ir

class FieldLit(AST):
	def __init__(self, nameTok, expr, span):
		super().__init__(span)
		self.nameTok = nameTok
		self.name = nameTok.content
		self.expr = expr
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.expr.pretty(output)

class StructLit(ValueExpr):
	def __init__(self, path, fields, span):
		super().__init__(span)
		self.path = path
		self.anon = path == None
		self.nameTok = path[-1] if path else None
		self.name = self.nameTok.content if path else None
		self.fields = fields
		self.typeModifiers = TypeModifiers(False)

	def analyze(expr, state, implicitType):
		fieldDict =  None
		uninitFields = set()
		resolvedType = None
		if expr.path:
			resolvedType = state.lookupSymbol(expr, True)
			if resolvedType == None:
				resolvedType = Type(expr.name, 0)
				return
		elif implicitType:
			resolvedType = implicitType
		
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
			
			state.analyzeNode(fieldInit.expr, fieldType)
			if fieldType and not typesMatch(fieldType, fieldInit.expr.type):
				logError(state, fieldInit.expr.span, 
					'expected type {}, found {}'.format(fieldType, fieldInit.expr.type))
		
		if resolvedType == None:
			fieldTypes = [field.expr.type for field in expr.fields]
			fieldNames = [field.name for field in expr.fields]
			layout = state.generateFieldLayout(fieldTypes, fieldNames)
			resolvedType = StructType(None, None, layout.align, layout.byteSize, layout.fields)
		
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
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Res(ast, fType))
		state.initStructFields(ast, 0)
	
	def pretty(self, output, indent=0):
		if self.path:
			output.write(self.path[0].content, indent)
			for p in self.path[1:]:
				output.write('::')
				output.write(p.content)
		output.write('\n')
		for field in self.fields[:-1]:
			field.pretty(output, indent + 1)
			output.write('\n')
		if len(self.fields) > 0:
			self.fields[-1].pretty(output, indent + 1)
