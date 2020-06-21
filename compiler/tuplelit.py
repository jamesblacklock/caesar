from .ast   import ValueExpr
from .types import typesMatch, Void, TupleType, ArrayType
from .      import ir

class TupleLit(ValueExpr):
	def __init__(self, values, span, typeSymbol=None):
		super().__init__(span)
		self.values = values
		self.type = typeSymbol
	
	def analyze(tup, state, implicitType):
		requireTypeMatch = False
		if tup.type:
			expectedTypes = [f.type for f in tup.type.fields]
			requireTypeMatch = True
		elif implicitType and implicitType.isCompositeType:
			expectedTypes = [f.type for f in implicitType.fields]
		else:
			expectedTypes = [None for _ in tup.values]
		
		if requireTypeMatch and len(tup.values) != len(expectedTypes):
			logError(state, tup.span, 'expected {} initializers for type `{}`, found {}'.format(
				len(expectedTypes), tup.type.name, len(tup.values)))
			while len(expectedTypes) < len(tup.values):
				expectedTypes.append(None)
		
		resolvedTypes = []
		values = []
		for (expr, t) in zip(tup.values, expectedTypes):
			expr = state.analyzeNode(expr, t)
			if requireTypeMatch:
				expr = state.typeCheck(expr, t)
			values.append(expr)
			resolvedTypes.append(expr.type)
		
		tup.values = values
		
		if not tup.type:
			layout = state.generateFieldLayout(resolvedTypes)
			tup.type = TupleType(layout.align, layout.byteSize, layout.fields)
	
	def accessSymbols(self, scope):
		for value in self.values:
			value.accessSymbols(scope)
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Res(ast, fType))
		state.initCompositeFields(ast, 0)
	
	def pretty(self, output, indent=0):
		output.write('(', indent)
		for e in self.values[:-1]:
			e.pretty(output)
			output.write(', ', indent)
		if len(self.values) > 0:
			self.values[-1].pretty(output)
			if len(self.values) == 1:
				output.write(',')
		output.write(')')

class ArrayLit(ValueExpr):
	def __init__(self, values, span):
		super().__init__(span)
		self.values = values

	def analyze(arr, state, implicitType):
		implicitElementType = Void
		count = 0
		if implicitType and implicitType.isCompositeType and len(implicitType.fields) > 0:
			implicitElementType = implicitType.fields[0].type
			count = len(implicitType.fields)
		
		values = []
		resolvedElementType = implicitElementType
		if len(arr.values) > 0:
			val = state.analyzeNode(arr.values[0], implicitElementType)
			values.append(val)
			resolvedElementType = arr.values[0].type
		
		for expr in arr.values[1:]:
			val = state.analyzeNode(expr, resolvedElementType)
			values.append(val)
			if not typesMatch(resolvedElementType, expr.type):
				logError(state, expr.span, 'expected {}, found {}'.format(resolvedElementType, expr.type))
		
		arr.values = values
		count = max(len(arr.values), count)
		arr.type = ArrayType(resolvedElementType, count)
	
	def accessSymbols(self, scope):
		for value in self.values:
			value.accessSymbols(scope)
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Res(ast, fType))
		state.initCompositeFields(ast, 0)
	
	def pretty(self, output, indent=0):
		output.write('[', indent)
		for e in self.values[:-1]:
			e.pretty(output)
			output.write(', ', indent)
		if len(self.values) > 0:
			self.values[-1].pretty(output)
		output.write(']')
