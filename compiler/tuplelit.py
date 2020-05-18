from .ast   import ValueExpr
from .types import typesMatch, Void, TupleType, ArrayType
from .      import ir

class TupleLit(ValueExpr):
	def __init__(self, values, span):
		super().__init__(span)
		self.values = values
	
	def analyze(tup, state, implicitType):
		implicitTypes = [None for _ in tup.values]
		if implicitType and implicitType.isCompositeType:
			for i in range(0, min(len(implicitType.fields), len(tup.values))):
				implicitTypes[i] = implicitType.fields[i].type
		
		resolvedTypes = []
		for (i, (expr, t)) in enumerate(zip(tup.values, implicitTypes)):
			expr = state.analyzeNode(expr, t)
			tup.values[i] = expr
			resolvedTypes.append(expr.type)
		
		layout = state.generateFieldLayout(resolvedTypes)
		tup.type = TupleType(layout.align, layout.byteSize, layout.fields)
	
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
		
		resolvedElementType = implicitElementType
		if len(arr.values) > 0:
			state.analyzeNode(arr.values[0], implicitElementType)
			resolvedElementType = arr.values[0].type
		
		for expr in arr.values[1:]:
			state.analyzeNode(expr, resolvedElementType)
			if not typesMatch(resolvedElementType, expr.type):
				logError(state, expr.span, 'expected {}, found {}'.format(resolvedElementType, expr.type))
		
		count = max(len(arr.values), count)
		arr.type = ArrayType(resolvedElementType, count)
	
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
