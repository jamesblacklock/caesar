from .ast   import ValueExpr
from .      import access, block
from .types import typesMatch, PtrType
from .ir    import FundamentalType, Imm, Add, Deref, Call, Field, IExtend, Extend, FExtend, IPTR, F64
from .log   import logError

class FnCall(ValueExpr):
	def __init__(self, expr, args, span):
		super().__init__(span)
		self.isMethodCall = False
		self.expr = expr
		self.args = args
		self.isDrop = False
		self.selfExpr = None
	
	def analyze(fnCall, state, implicitType):
		selfExpr = None
		failed = False
		if fnCall.isMethodCall:
			selfExpr = access.SymbolAccess.analyzeSymbolAccess(state, fnCall.args[0], noAccess=True)
			fnCall.args[0] = selfExpr
			if not selfExpr.type:
				failed = True
			else:
				name = fnCall.expr.path[0].content
				if name not in selfExpr.type.symbolTable:
					logError(state, fnCall.expr.span, 'type `{}` has no method `{}`'.format(selfExpr.type, name))
					failed = True
				else:
					if selfExpr.type.isTraitType:
						fnCall.selfExpr = selfExpr
					
					symbol = selfExpr.type.symbolTable[name]
					fnCall.expr = access.SymbolRead(fnCall.expr.span)
					fnCall.expr.symbol = symbol
					fnCall.expr.type = symbol.type
					fnCall.expr.ref = True
		
		fnType = None
		if not failed:
			fnCall.expr = state.analyzeNode(fnCall.expr)
			if fnCall.expr.type:
				fnType = fnCall.expr.type
				if not fnType.isFnType:
					logError(state, fnCall.expr.span, 'the expression cannot be called as a function')
				else:
					if fnCall.isMethodCall:
						if len(fnType.params) > 0 and fnType.params[0].type.isPtrType:
							selfExpr.type = PtrType(selfExpr.type, 1, fnType.params[0].type.mut)
							if type(selfExpr) == block.Block:
								read = selfExpr.exprs[-1]
								read.type = selfExpr.type
								selfExpr = read
							
							assert type(selfExpr) == access.SymbolRead
							
							if selfExpr.deref:
								selfExpr.deref -= 1
							else:
								selfExpr.ref = False
								selfExpr.addr = True
								selfExpr.borrows = {selfExpr}
							selfExpr.dropBlock = state.scope.scopeLevelDropBlock
						elif type(selfExpr) == block.Block:
							selfExpr = selfExpr.exprs[-1]
						
						state.scope.accessSymbol(selfExpr)
					
					fnCall.type = fnType.returnType
					if fnType.unsafe and not state.scope.allowUnsafe:
						logError(state, fnCall.expr.span, 'unsafe function called in a safe context')
					
					if len(fnCall.args) < len(fnType.params) or \
						not fnType.cVarArgs and len(fnCall.args) > len(fnType.params):
						logError(state, fnCall.span, 
							'function called with wrong number of arguments (expected {}, found {})'
								.format(len(fnType.params), len(fnCall.args)))
		
		params = list(fnType.params) if fnType else []
		for _ in range(len(params), len(fnCall.args)):
			params.append(None)
		
		isSelfArg = fnCall.isMethodCall
		cVarArgs = fnType.cVarArgs if fnType else False
		args = []
		for (param, arg) in zip(params, fnCall.args):
			expectedType = param.type if param else None
			
			if not isSelfArg:
				arg = state.analyzeNode(arg, expectedType)
				if arg.type and not arg.type.isPrimitiveType:
					if expectedType == None and cVarArgs:
						logError(state, arg.span, 
							'type {} cannot be used as a C variadic argument'.format(arg.type))
			
			if arg.type and arg.type.isPtrType and arg.typeModifiers and arg.typeModifiers.uninit:
				if param:
					logError(state, arg.span, 'reference to uninit symbol passed as parameter `{}`'.format(param.name))
				else:
					assert cVarArgs
					logError(state, arg.span, 'reference to uninit symbol passed as C variadic argument')
			
			arg = state.typeCheck(arg, expectedType)
			args.append(arg)
			isSelfArg = False
		
		fnCall.args = args
	
	def writeIR(ast, state):
		normalArgs = ast.args
		cVarArgs = []
		numParams = len(ast.expr.type.params)
		if ast.expr.type.cVarArgs:
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
		
		if ast.selfExpr:
			index = ast.selfExpr.type.baseType.mod.decls.index(ast.expr.symbol)
			state.appendInstr(Imm(ast, IPTR, IPTR.byteSize))
			state.appendInstr(Field(ast, numParams, IPTR))
			if index > 0:
				state.appendInstr(Imm(ast, IPTR, IPTR.byteSize * index))
				state.appendInstr(Add(ast))
			state.appendInstr(Deref(ast, IPTR))
		else:
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
