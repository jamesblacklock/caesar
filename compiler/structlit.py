from .ast   import AST, ValueExpr, TypeModifiers
from .types import getValidAssignType
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
		self.nameTok = path[-1]
		self.name = self.nameTok.content
		self.fields = fields
		self.typeModifiers = TypeModifiers(False)
		self.isConst = True
		for init in self.fields:
			if not init.expr.isConst:
				self.isConst = False
				break

	def analyze(expr, state, implicitType):
		resolvedType = state.lookupSymbol(expr, True)
		
		if resolvedType == None:
			resolvedType = Type(expr.name, 0)
			return
		
		if not resolvedType.isStructType:
			logError(state, expr.nameTok.span, 'type `{}` is not a struct type'.format(expr.name))
			return
		
		fieldDict = resolvedType.fieldDict
		uninitFields = { f for f in resolvedType.fields }
		initFields = {}
		for fieldInit in expr.fields:
			fieldSymbol = None
			fieldType = None
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
					'struct `{}` has no field `{}`'.format(resolvedType.name, fieldInit.name))
			
			state.analyzeNode(fieldInit.expr, fieldType)
			if not getValidAssignType(fieldType, fieldInit.expr.type):
				logError(state, fieldInit.expr.span, 
					'expected type {}, found {}'.format(fieldType, fieldInit.expr.type))
				
		
		# uninit = [field for field in fieldDict if field not in fieldNames]
		# if len(uninit) > 0:
		# 	fieldsStr = None
		# 	if len(uninit) == 1:
		# 		fieldsStr = uninit[0]
		# 	elif len(uninit) == 2:
		# 		fieldsStr = '{} and {}'.format(*uninit)
		# 	elif len(uninit) < 5:
		# 		fieldsStr = '{}, and {}'.format(', '.join(uninit[:-1]), uninit[-1])
		# 	else:
		# 		fieldsStr = '{}, and {} other fields'.format(', '.join(uninit[:3]), len(uninit) - 3)
		# 	message = 'missing {} {} in initializer of `{}`'.format(
		# 		'field' if len(uninit) == 1 else 'fields',
		# 		fieldsStr,
		# 		expr.name
		# 	)
		# 	logError(state, expr.nameTok.span, message)
		
		expr.type = resolvedType
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Struct(ast, fType))
		state.initStructFields(ast, 0)
	
	def pretty(self, output, indent=0):
		output.write(self.path[0].content, indent)
		for p in self.path[1:]:
			output.write('::')
			output.write(p.content)
		output.write('\n')
		for field in self.fields[:-1]:
			field.pretty(output, indent + 1)
			output.write('\n')
		if len(self.fields) > 1:
			self.fields[-1].pretty(output, indent + 1)