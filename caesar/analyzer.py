from .parser             import CConv, FnDeclAST, LetAST, FnCallAST, ReturnAST, IfAST, TypeRefAST, \
                                StrLitAST, IntLitAST, BoolLitAST, TupleLitAST, ValueRefAST, \
								InfixOpAST, FnCallAST, IfAST, CoercionAST, ModAST, ValueExprAST, \
								BlockAST
from .                   import types
from .types              import typesMatch
from .err                import logError

def ffiAttr(decl, params):
	if len(params) != 1 or params[0].content != '"C"':
		raise RuntimeError('FFI currently only supports the "C" convention')
	if type(decl) != FnDeclAST:
		raise RuntimeError('FFI attribute can only be applied to functions')
	
	decl.cconv = CConv.C
	decl.mangledName = '_{}'.format(decl.name)

builtinAttrs = {
	'FFI': ffiAttr
}

def invokeAttrs(state, decl):
	for attr in decl.attrs:
		if attr.name not in builtinAttrs:
			logError(state, attr.span, 'attribute "{}" does not exist'.format(attr.name))
			continue
		
		builtinAttrs[attr.name](decl, attr.args)

def lookupSymbol(state, scope, symbolRef, inTypePosition):
	path = symbolRef.path
	symbol = None
	
	while scope != None:
		if path[0] in scope.symbolTable:
			symbol = scope.symbolTable[path[0]]
			break
		scope = scope.parentScope
	
	if symbol != None:
		for name in path[1:]:
			if type(symbol) != ModAST:
				logError(state, symbolRef.span, '`{}` is not a module'.format(symbol.name))
				symbol = None
				break
			
			if name not in symbol.symbolTable:
				return None
			symbol = symbol.symbolTable[name]
	
	if inTypePosition:
		if symbol == None:
			logError(state, symbolRef.span, 'cannot resolve type `{}`'.format(symbolRef.name))
		elif type(symbol) != types.ResolvedType:
			logError(state, symbolRef.span, '`{}` is not a type'.format(symbolRef.name))
			symbol = None
	else:
		if symbol == None:
			logError(state, symbolRef.span, 'cannot resolve the symbol `{}`'.format(symbolRef.name))
		elif type(symbol) == types.ResolvedType:
			logError(state, symbolRef.span, 'found a type reference where a value was expected')
			symbol = None
		elif type(symbol) == ModAST:
			logError(state, symbolRef.span, 'found a module name where a value was expected')
			symbol = None
	
	return symbol

def resolveValueExprType(state, scope, expr):
	if type(expr) == StrLitAST:
		expr.resolvedType = types.ResolvedPtrType(types.Byte, 1)
	elif type(expr) == BoolLitAST:
		expr.resolvedType = types.Bool
	elif type(expr) == IntLitAST:
		if expr.suffix == 'i8':
			expr.resolvedType = types.Int8
		elif expr.suffix == 'u8':
			expr.resolvedType = types.UInt8
		elif expr.suffix == 'i16':
			expr.resolvedType = types.Int16
		elif expr.suffix == 'u16':
			expr.resolvedType = types.UInt16
		elif expr.suffix == 'i32':
			expr.resolvedType = types.Int32
		elif expr.suffix == 'u32':
			expr.resolvedType = types.UInt32
		elif expr.suffix == 'i64':
			expr.resolvedType = types.Int64
		elif expr.suffix == 'u64':
			expr.resolvedType = types.UInt64
		elif expr.suffix == 'sz':
			expr.resolvedType = types.ISize
		elif expr.suffix == 'usz':
			expr.resolvedType = types.USize
		else:
			expr.resolvedType = types.ResolvedMultiIntType.fromValue(int(expr.value))
	elif type(expr) == BlockAST:
		typeCheckBlock(state, scope, expr.exprs)
		expr.resolvedType = getBlockType(expr.exprs)
	elif type(expr) == TupleLitAST:
		raise RuntimeError('unimplemented!')
	elif type(expr) == ValueRefAST:
		typeCheckValueRef(state, scope, expr)
	elif type(expr) == InfixOpAST:
		typeCheckInfixOp(state, scope, expr)
	elif type(expr) == FnCallAST:
		typeCheckFnCall(state, scope, expr)
	elif type(expr) == IfAST:
		typeCheckIf(state, scope, expr)
	elif type(expr) == CoercionAST:
		typeCheckCoercion(state, scope, expr)
	else:
		assert 0

def resolveTypeRefType(state, scope, typeRef):
	resolvedType = lookupSymbol(state, scope, typeRef, True)
	
	if typeRef.indirectionLevel > 0:
		resolvedType = types.ResolvedPtrType(resolvedType, typeRef.indirectionLevel)
	
	typeRef.resolvedType = resolvedType

def typeCheckValueRef(state, scope, valueRef):
	symbol = lookupSymbol(state, scope, valueRef, False)
	valueRef.resolvedType = symbol.resolvedSymbolType if symbol else None

def typeCheckInfixOp(state, scope, infixOp):
	opErr = lambda: \
		logError(state, infixOp.opSpan, 'invalid operand types for operator `{}`'.format(infixOp.op.desc()))
	
	resolveValueExprType(state, scope, infixOp.l)
	resolveValueExprType(state, scope, infixOp.r)
	
	if not infixOp.l.resolvedType or not infixOp.r.resolvedType:
		return
	
	if infixOp.l.resolvedType.isIntType and infixOp.r.resolvedType.isIntType:
		lType = infixOp.l.resolvedType
		rType = infixOp.r.resolvedType
		
		possibleTypes = types.ResolvedMultiIntType.inCommon(lType, rType)
		
		if len(possibleTypes) > 0:
			if infixOp.op in types.ARITHMETIC_OPS:
				if len(possibleTypes) == 1:
					infixOp.resolvedType = possibleTypes[0]
					return
				else:
					infixOp.resolvedType = types.ResolvedMultiIntType(possibleTypes)
					return
			elif infixOp.op in types.CMP_OPS:
				infixOp.resolvedType = types.Bool
				return
	
	opErr()

def typeCheckFnSig(state, fnDecl):
	if fnDecl.returnType == None:
		fnDecl.returnType = TypeRefAST('Void', 0, fnDecl.span)
	
	resolveTypeRefType(state, fnDecl, fnDecl.returnType)
	
	for param in fnDecl.params:
		resolveTypeRefType(state, fnDecl, param.typeRef)
		if param.name in fnDecl.symbolTable:
			logError(state, param.span, 'duplicate parameter name')
		else:
			fnDecl.symbolTable[param.name] = param
	
	resolvedParamTypes = [param.typeRef.resolvedType for param in fnDecl.params]
	fnDecl.resolvedSymbolType = types.ResolvedFnType(resolvedParamTypes, fnDecl.cVarArgs, fnDecl.returnType.resolvedType)
	
	if fnDecl.cVarArgs and fnDecl.cconv != CConv.C:
		logError(state, fnDecl.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')

def typeCheckLet(state, scope, letExpr):
	if type(scope) != FnDeclAST:
		fnScope = scope.parentScope
		while True:
			if letExpr.name in scope.symbolTable:
				logError(state, letExpr.span, '`{}` has already been declared in an outer scope'.format(letExpr.name))
				break
			
			if type(fnScope) == FnDeclAST:
				break
	
	scope.symbolTable[letExpr.name] = letExpr
	
	resolveValueExprType(state, scope, letExpr.expr)
	
	if letExpr.typeRef:
		resolveTypeRefType(state, scope, letExpr.typeRef)
		letExpr.resolvedSymbolType = letExpr.typeRef.resolvedType
		
		if not typesMatch(letExpr.resolvedSymbolType, letExpr.expr.resolvedType):
			logError(state, letExpr.expr.span, 'expected type {}, found {}'
				.format(letExpr.resolvedSymbolType, letExpr.expr.resolvedType))
	else:
		letExpr.resolvedSymbolType = letExpr.expr.resolvedType

def typeCheckFnCall(state, scope, fnCallExpr):
	resolveValueExprType(state, scope, fnCallExpr.expr)
	fnType = fnCallExpr.expr.resolvedType
	if fnType == None:
		return
	
	if not fnType.isFnType:
		logError(state, fnCallExpr.expr.span, 'the expression cannot be called as a function')
		return
	
	fnCallExpr.resolvedType = fnType.resolvedReturnType
	
	for arg in fnCallExpr.args:
		resolveValueExprType(state, scope, arg)
	
	if len(fnCallExpr.args) < len(fnType.resolvedParamTypes) or \
		not fnType.cVarArgs and len(fnCallExpr.args) > len(fnType.resolvedParamTypes):
		logError(state, fnCallExpr.span, 
			'function called with wrong number of arguments (expected {}, found {})'.format(len(fnType.resolvedParamTypes), len(fnCallExpr.args)))
		return
	
	for (expected, found) in zip(fnType.resolvedParamTypes, fnCallExpr.args):
		if not typesMatch(expected, found.resolvedType):
			logError(state, found.span, 'expected type {}, found {}'.format(expected, found.resolvedType))

def typeCheckReturn(state, scope, retExpr):
	fnScope = scope
	while type(scope) != FnDeclAST:
		scope = scope.parentScope
	
	resolveValueExprType(state, scope, retExpr.expr)
	if not typesMatch(scope.resolvedSymbolType.resolvedReturnType, retExpr.expr.resolvedType):
		logError(state, retExpr.expr.span, 'invalid return type (expected {}, found {})'
			.format(scope.resolvedSymbolType.resolvedReturnType, retExpr.expr.resolvedType))

def getBlockType(block):
	if len(block) == 0:
		return types.Void
	
	lastExpr = block[-1]
	if not isinstance(lastExpr, ValueExprAST):
		return types.Void
	
	return lastExpr.resolvedType

def typeCheckIf(state, scope, ifExpr):
	resolveValueExprType(state, scope, ifExpr.expr)
	if ifExpr.expr.resolvedType != types.Bool:
		logError(state, ifExpr.expr.span, 
			'condition type must be Bool (found {})'.format(ifExpr.expr.resolvedType))
	
	typeCheckBlock(state, scope, ifExpr.ifBlock)
	resolvedType = getBlockType(ifExpr.ifBlock)
	
	if ifExpr.elseBlock:
		typeCheckBlock(state, scope, ifExpr.elseBlock)
		elseResolvedType = getBlockType(ifExpr.elseBlock)
		if not typesMatch(resolvedType, elseResolvedType):
			if typesMatch(elseResolvedType, resolvedType):
				resolvedType = elseResolvedType
			else:
				resolvedType = types.ResolvedOptionType(resolvedType, elseResolvedType)
	
	ifExpr.resolvedType = resolvedType

def typeCheckCoercion(state, scope, asExpr):
	resolveValueExprType(state, scope, asExpr.expr)
	resolveTypeRefType(state, scope, asExpr.typeRef)
	
	asExpr.resolvedType = asExpr.typeRef.resolvedType
	
	if not types.canCoerce(asExpr.expr.resolvedType, asExpr.typeRef.resolvedType):
		logError(state, asExpr.span, 'cannot coerce from {} to {}'
			.format(asExpr.expr.resolvedType, asExpr.typeRef.resolvedType))

def typeCheckBlock(state, scope, block):
	for expr in block:
		if type(expr) == LetAST:
			typeCheckLet(state, scope, expr)
		elif type(expr) == ReturnAST:
			typeCheckReturn(state, scope, expr)
		elif isinstance(expr, ValueExprAST):
			resolveValueExprType(state, scope, expr)
		else:
			assert 0

def typeCheckFnBody(state, fnDecl):
	if fnDecl.body == None:
		return
	
	typeCheckBlock(state, fnDecl, fnDecl.body)
	# TODO: escape analysis

def typeCheckMod(state, mod):
	for decl in mod.importDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.staticDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		typeCheckFnSig(state, decl)
	
	for decl in mod.modDecls:
		typeCheckMod(state, decl)
	
	for decl in mod.fnDecls:
		typeCheckFnBody(state, decl)

class AnalyzerState:
	def __init__(self, mod):
		self.mod = mod
		self.failed = False

def analyze(mod):
	state = AnalyzerState(mod)
	
	typeCheckMod(state, mod)
	
	if state.failed:
		exit(1)
	
	return mod
