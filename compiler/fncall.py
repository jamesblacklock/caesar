from .ast   import ValueExpr
from .types import typesMatch
from .ir    import FundamentalType, Call, IExtend, Extend, FExtend, IPTR, F64
from .log   import logError

class FnCall(ValueExpr):
	def __init__(self, expr, args, span):
		super().__init__(span)
		self.expr = expr
		self.args = args
		self.isDrop = False
	
	def analyze(fnCall, state, implicitType):
		fnCall.expr = state.analyzeNode(fnCall.expr)
		fnType = fnCall.expr.type
		if fnType == None:
			return
		elif not fnType.isFnType:
			logError(state, fnCall.expr.span, 'the expression cannot be called as a function')
			return
		
		fnCall.type = fnType.returnType
		
		if fnType.unsafe and not state.scope.allowUnsafe:
			logError(state, fnCall.expr.span, 'unsafe function called in a safe context')
		
		if len(fnCall.args) < len(fnType.params) or \
			not fnType.cVarArgs and len(fnCall.args) > len(fnType.params):
			logError(state, fnCall.span, 
				'function called with wrong number of arguments (expected {}, found {})'
					.format(len(fnType.params), len(fnCall.args)))
			return
		
		for (i, (expected, arg)) in enumerate(zip(fnType.params, fnCall.args)):
			arg = state.analyzeNode(arg, expected.type)
			fnCall.args[i] = arg
			if arg.type and not typesMatch(expected.type, arg.type):
				logError(state, arg.span, 
					'expected type {}, found {}'.format(expected.type, arg.type))
		
		if fnType.cVarArgs and len(fnCall.args) > len(fnType.params):
			offset = len(fnType.params)
			for (i, arg) in enumerate(fnCall.args[offset:]):
				arg = state.analyzeNode(arg)
				fnCall.args[i + offset] = arg
				if arg.type and not arg.type.isPrimitiveType:
					logError(state, arg.span, 
						'type {} cannot be used as a C variadic argument'.format(arg.type))
	
	def writeIR(ast, state):
		normalArgs = ast.args
		cVarArgs = []
		if ast.expr.type.cVarArgs:
			numParams = len(ast.expr.type.params)
			normalArgs = ast.args[:numParams]
			cVarArgs = ast.args[numParams:]
		
		for expr in normalArgs:
			expr.writeIR(state)
			if ast.isDrop and state.loopInfo:
				state.loopInfo.droppedSymbols.add(expr.symbol)
		
		for expr in cVarArgs:
			expr.writeIR(state)
			if expr.type.isVoidType:
				continue
			
			fType = FundamentalType.fromResolvedType(expr.type)
			if fType.isFloatType and fType.byteSize != 8:
				assert fType.byteSize == 4
				state.appendInstr(FExtend(ast, F64))
			elif fType.byteSize < 4:
				if expr.type.isSigned:
					state.appendInstr(IExtend(ast, IPTR))
				else:
					state.appendInstr(Extend(ast, IPTR))
		
		ast.expr.writeIR(state)
		
		retType = FundamentalType.fromResolvedType(ast.type) if not ast.type.isVoidType else None
		state.appendInstr(Call(ast, len(ast.args), retType, ast.expr.type.cVarArgs))
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write('(')
		if len(self.args) > 0:
			self.args[0].pretty(output)
			for arg in self.args[1:]:
				output.write(', ')
				arg.pretty(output)
		output.write(')')
