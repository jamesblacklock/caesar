from .ast       import ValueExpr
from .          import access, block, valueref, enumdecl, primitive
from .structlit import StructLit, FieldLit
from .tuplelit  import TupleLit
from .types     import typesMatch, PtrType, TypeSymbol
from .ir        import FundamentalType, Imm, Add, Deref, Call, Field, IExtend, Extend, FExtend, IPTR, F64
from .log       import logError

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
			selfExpr = access.SymbolAccess.analyzeSymbolAccess(state, fnCall.args[0])
			fnCall.args[0] = selfExpr
			if not selfExpr.type:
				failed = True
			else:
				name = fnCall.expr.path[0].content
				symbol = None
				if name in selfExpr.type.symbolTable:
					symbol = selfExpr.type.symbolTable[name]
				if selfExpr.type.extern and not symbol.pub:
					symbol = None
				if symbol == None:
					logError(state, fnCall.expr.span, 'cannot resolve the method `{}` for type `{}`'.format(name, selfExpr.type))
					failed = True
				else:
					if selfExpr.type.isTraitType:
						fnCall.selfExpr = selfExpr
					fnCall.expr = access.SymbolRead(fnCall.expr.span)
					fnCall.expr.symbol = symbol
					fnCall.expr.type = symbol.type
					fnCall.expr.ref = True
		elif type(fnCall.expr) == valueref.ValueRef:
			symbolTable = implicitType.symbolTable if implicitType and implicitType.isEnumType else None
			symbol = state.lookupSymbol(fnCall.expr.path, symbolTable)
			if not symbol:
				failed = True
			else:
				enumType = None
				variant = None
				if type(symbol) == enumdecl.VariantDecl and symbol.type:
					enumType = symbol.enumType
					variant = symbol
					symbol = symbol.type
				if isinstance(symbol, TypeSymbol):
					if symbol.isTupleType:
						expr = TupleLit(fnCall.args, fnCall.span, typeSymbol=symbol)
						if enumType:
							dataField = FieldLit(None, expr, expr.span, name='$' + variant.name)
							dataStruct = StructLit(None, True, [dataField], expr.span, typeSymbol=enumType.structType.fields[0].type)
							tagValue = primitive.IntLit(None, False, expr.span, value=variant.tag.data)
							fields = [
								FieldLit(None, dataStruct, expr.span, name='$data'), 
								FieldLit(None, tagValue, expr.span, name='$tag')
							]
							expr = StructLit(None, False, fields, expr.span, typeSymbol=enumType)
						return state.analyzeNode(expr)
					else:
						logError(state, fnCall.expr.span, 'the expression cannot be called as a function')
						failed = True
		
		fnType = None
		if not failed:
			fnCall.expr = state.analyzeNode(fnCall.expr, implicitType)
			if fnCall.expr.type:
				if not fnCall.expr.type.isFnType:
					logError(state, fnCall.expr.span, 'the expression cannot be called as a function')
				else:
					fnType = fnCall.expr.type
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
				if arg.type and not arg.type.isPrimitiveType and not arg.type.isUnknown:
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
	
	def accessSymbols(self, scope):
		self.expr.accessSymbols(scope)
		for arg in self.args:
			arg.accessSymbols(scope)
	
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
			state.appendInstr(Imm(ast, IPTR, IPTR.byteSize))
			state.appendInstr(Field(ast, numParams, IPTR))
			index = 1 + ast.selfExpr.type.baseType.mod.decls.index(ast.expr.symbol)
			state.appendInstr(Imm(ast, IPTR, IPTR.byteSize * index))
			state.appendInstr(Add(ast))
			state.appendInstr(Deref(ast, IPTR))
		else:
			ast.expr.writeIR(state)
		
		retType = FundamentalType.fromResolvedType(ast.type) if not ast.type.isVoidType else None
		state.appendInstr(Call(ast, len(ast.args), retType, ast.expr.type.cVarArgs))
	
	def prettyArg(self, arg, output, indent):
		if type(arg) == block.Block:
			output.write('\n')
			arg.pretty(output, indent + 1)
		else:
			arg.pretty(output)
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write('(')
		if len(self.args) > 0:
			self.prettyArg(self.args[0], output, indent)
			for arg in self.args[1:]:
				output.write(', ')
				self.prettyArg(arg, output, indent)
		output.write(')')
