from .                   import ast
from .ast                import CConv, FnDeclAST, LetAST, FnCallAST, ReturnAST, IfAST, TypeRefAST, \
                                StrLitAST, IntLitAST, BoolLitAST, TupleLitAST, ValueRefAST, \
								InfixOpAST, FnCallAST, IfAST, CoercionAST, ModAST, ValueExprAST, \
								BlockAST, AsgnAST, WhileAST, DerefAST, IndexOpAST, VoidAST, \
								AddressAST, FloatLitAST
from .                   import types
from .types              import getValidAssignType, ResolvedType
from .err                import logError, logWarning
from .span               import Span

def ffiAttr(decl, params):
	if len(params) != 1 or params[0].content != '"C"':
		raise RuntimeError('FFI currently only supports the "C" convention')
	if type(decl) != FnDeclAST:
		raise RuntimeError('FFI attribute can only be applied to functions')
	
	decl.cconv = CConv.C

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
			if state and type(symbol) != ModAST:
				logError(state, symbolRef.span, '`{}` is not a module'.format(symbol.name))
				symbol = None
				break
			
			if name not in symbol.symbolTable:
				return None
			symbol = symbol.symbolTable[name]
	
	if state:
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

def resolveValueExprType(state, scope, expr, expectedType=None):
	if type(expr) == StrLitAST:
		expr.resolvedType = types.ResolvedPtrType(types.Byte, 1)
	elif type(expr) == BoolLitAST:
		expr.resolvedType = types.Bool
	elif type(expr) == IntLitAST:
		typeCheckIntLit(state, scope, expr, expectedType)
	elif type(expr) == FloatLitAST:
		typeCheckFloatLit(state, scope, expr, expectedType)
	elif type(expr) == BlockAST:
		typeCheckBlock(state, scope, expr, expectedType)
	elif type(expr) == DerefAST:
		typeCheckDeref(state, scope, expr)
	elif type(expr) == AddressAST:
		typeCheckAddress(state, scope, expr, expectedType)
	elif type(expr) == TupleLitAST:
		raise RuntimeError('unimplemented!')
	elif type(expr) == ValueRefAST:
		typeCheckValueRef(state, scope, expr)
	elif type(expr) == InfixOpAST:
		typeCheckInfixOp(state, scope, expr, expectedType)
	elif type(expr) == FnCallAST:
		typeCheckFnCall(state, scope, expr)
	elif type(expr) == IndexOpAST:
		typeCheckIndex(state, scope, expr)
	elif type(expr) == IfAST:
		typeCheckIf(state, scope, expr, expectedType)
	elif type(expr) == CoercionAST:
		typeCheckCoercion(state, scope, expr)
	elif type(expr) == VoidAST:
		expr.resolvedType = types.Void
	else:
		assert 0

def resolveTypeRefType(state, scope, typeRef):
	resolvedType = lookupSymbol(state, scope, typeRef, True)
	
	if resolvedType == None:
		resolvedType = ResolvedType(typeRef.name, 0)
	
	if typeRef.indirectionLevel > 0:
		resolvedType = types.ResolvedPtrType(resolvedType, typeRef.indirectionLevel)
	
	typeRef.resolvedType = resolvedType

def typeCheckValueRef(state, scope, valueRef):
	symbol = lookupSymbol(state, scope, valueRef, False)
	valueRef.resolvedType = symbol.resolvedSymbolType if symbol else None

def typeCheckIntLit(state, scope, lit, expectedType):
	if lit.suffix == 'i8':
		lit.resolvedType = types.Int8
	elif lit.suffix == 'u8':
		lit.resolvedType = types.UInt8
	elif lit.suffix == 'i16':
		lit.resolvedType = types.Int16
	elif lit.suffix == 'u16':
		lit.resolvedType = types.UInt16
	elif lit.suffix == 'i32':
		lit.resolvedType = types.Int32
	elif lit.suffix == 'u32':
		lit.resolvedType = types.UInt32
	elif lit.suffix == 'i64':
		lit.resolvedType = types.Int64
	elif lit.suffix == 'u64':
		lit.resolvedType = types.UInt64
	elif lit.suffix == 'sz':
		lit.resolvedType = types.ISize
	elif lit.suffix == 'usz':
		lit.resolvedType = types.USize
	elif expectedType and expectedType.isIntType:
		lit.resolvedType = expectedType
	elif types.canAccommodate(types.Int32, lit.value):
		lit.resolvedType = types.Int32
	elif types.canAccommodate(types.Int64, lit.value) or lit.value < 0:
		lit.resolvedType = types.Int64
	else:
		lit.resolvedType = types.UInt64
	
	if not types.canAccommodate(lit.resolvedType, lit.value):
		logError(state, lit.span, 'integer value out of range for type {}'.format(lit.resolvedType))

def typeCheckFloatLit(state, scope, lit, expectedType):
	if lit.suffix == 'f32':
		lit.resolvedType = types.Float32
	elif lit.suffix == 'f64':
		lit.resolvedType = types.Float64
	elif expectedType and expectedType.isFloatType:
		lit.resolvedType = expectedType
	else:# elif types.canAccommodate(types.Float32, lit.value):
		lit.resolvedType = types.Float32
	# else:
	# 	lit.resolvedType = types.Float64
	
	# if not types.canAccommodate(lit.resolvedType, lit.value):
	# 	logError(state, lit.span, 'flaoting point value out of range for type {}'.format(lit.resolvedType))

def typeCheckAddress(state, scope, addr, expectedType):
	resolveValueExprType(state, scope, addr.expr, expectedType)
	if addr.expr.resolvedType == None:
		return
	
	baseType = addr.expr.resolvedType
	indLevel = 1
	if baseType.isPtrType:
		baseType = baseType.baseType
		indLevel = baseType.indLevel + 1
	
	addr.resolvedType = types.ResolvedPtrType(baseType, indLevel)

def typeCheckDeref(state, scope, deref):
	resolveValueExprType(state, scope, deref.expr)
	if deref.expr.resolvedType == None:
		return
	elif not deref.expr.resolvedType.isPtrType:
		logError(state, deref.span, 'cannot dereference: not a pointer')
	else:
		indLevel = deref.expr.resolvedType.indirectionLevel
		derefCount = deref.derefCount
		baseType = deref.expr.resolvedType.baseType
		if indLevel < derefCount:
			logError(state, deref.span, 'dereferenced too many times (max for type is {})'.format(indLevel))
		elif indLevel == derefCount:
			deref.resolvedType = baseType
		else:
			deref.resolvedType = types.ResolvedPtrType(baseType, indLevel - derefCount)

def typeCheckInfixOp(state, scope, infixOp, expectedType):
	opErr = lambda: \
		logError(state, infixOp.opSpan, 'invalid operand types for operator `{}`'.format(infixOp.op.desc()))
	
	lIndefinite = not types.hasDefiniteType(infixOp.l)
	rIndefinite = not types.hasDefiniteType(infixOp.r)
	if lIndefinite or rIndefinite:
		if not lIndefinite:
			resolveValueExprType(state, scope, infixOp.l, expectedType)
			resolveValueExprType(state, scope, infixOp.r, infixOp.l.resolvedType)
		elif not rIndefinite:
			resolveValueExprType(state, scope, infixOp.r, expectedType)
			resolveValueExprType(state, scope, infixOp.l, infixOp.r.resolvedType)
		elif types.canAccommodate(types.Int32, infixOp.l.value) and types.canAccommodate(types.Int32, infixOp.r.value):
			resolveValueExprType(state, scope, infixOp.r, types.Int32)
			resolveValueExprType(state, scope, infixOp.l, types.Int32)
		elif types.canAccommodate(types.Int64, infixOp.l.value) and types.canAccommodate(types.Int64, infixOp.r.value):
			resolveValueExprType(state, scope, infixOp.r, types.Int64)
			resolveValueExprType(state, scope, infixOp.l, types.Int64)
		else:
			resolveValueExprType(state, scope, infixOp.r, types.UInt64)
			resolveValueExprType(state, scope, infixOp.l, types.UInt64)
	else:
		resolveValueExprType(state, scope, infixOp.r, expectedType)
		resolveValueExprType(state, scope, infixOp.l, expectedType)
	
	if not infixOp.l.resolvedType or not infixOp.r.resolvedType:
		return
	
	if infixOp.l.resolvedType.isPtrType and infixOp.r.resolvedType.isIntType:
		if infixOp.op in ast.PTR_INT_OPS:
			infixOp.resolvedType = infixOp.l.resolvedType
			return
	elif infixOp.l.resolvedType.isIntType and infixOp.r.resolvedType.isPtrType:
		if infixOp.op in ast.INT_PTR_OPS:
			infixOp.resolvedType = infixOp.r.resolvedType
			return
	elif infixOp.l.resolvedType.isPtrType and infixOp.r.resolvedType.isPtrType:
		if infixOp.op in ast.PTR_PTR_OPS and getValidAssignType(infixOp.l.resolvedType, infixOp.r.resolvedType):
			infixOp.resolvedType = infixOp.l.resolvedType
			return
	elif infixOp.l.resolvedType.isIntType and infixOp.r.resolvedType.isIntType:
		lType = infixOp.l.resolvedType
		rType = infixOp.r.resolvedType
		
		if lType.byteSize == rType.byteSize and lType.isSigned == rType.isSigned:
			if infixOp.op in ast.ARITHMETIC_OPS or infixOp.op in ast.BITWISE_OPS:
				infixOp.resolvedType = lType
				return
			elif infixOp.op in ast.CMP_OPS:
				infixOp.resolvedType = types.Bool
				return
		
		if infixOp.op in ast.BITSHIFT_OPS:
			infixOp.resolvedType = lType
			return
	
	opErr()

def typeCheckIndex(state, scope, expr):
	resolveValueExprType(state, scope, expr.expr)
	resolveValueExprType(state, scope, expr.index, types.ISize)
	
	if not expr.expr.resolvedType.isPtrType:
		logError(state, expr.expr.span, 
			'base of index expression must be an pointer type (found {})'.format(expr.expr.resolvedType))
	
	if not expr.index.resolvedType.isIntType:
		logError(state, expr.index.span, 'index must be an integer (found {})'.format(expr.index.resolvedType))

def typeCheckFnSig(state, scope, fnDecl):
	fnDecl.parentScope = scope
	
	if fnDecl.returnType == None:
		fnDecl.returnType = TypeRefAST('Void', 0, fnDecl.span)
	
	resolveTypeRefType(state, fnDecl, fnDecl.returnType)
	
	for param in fnDecl.params:
		resolveTypeRefType(state, fnDecl, param.typeRef)
		param.resolvedSymbolType = param.typeRef.resolvedType
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
	
	if letExpr.typeRef:
		resolveTypeRefType(state, scope, letExpr.typeRef)
		letExpr.resolvedSymbolType = letExpr.typeRef.resolvedType
		
		if letExpr.expr:
			resolveValueExprType(state, scope, letExpr.expr, letExpr.resolvedSymbolType)
			assignType = getValidAssignType(letExpr.resolvedSymbolType, letExpr.expr.resolvedType)
			if assignType:
				letExpr.expr.resolvedType = assignType
			else:
				logError(state, letExpr.expr.span, 'expected type {}, found {}'
					.format(letExpr.resolvedSymbolType, letExpr.expr.resolvedType))
	elif letExpr.expr:
		resolveValueExprType(state, scope, letExpr.expr)
		letExpr.resolvedSymbolType = letExpr.expr.resolvedType
	else:
		logError(state, letExpr.expr.span, 'cannot infer type of `{}`'.format(letExpr.name))

def typeCheckFnCall(state, scope, fnCallExpr):
	resolveValueExprType(state, scope, fnCallExpr.expr)
	fnType = fnCallExpr.expr.resolvedType
	if fnType == None:
		return
	
	if not fnType.isFnType:
		logError(state, fnCallExpr.expr.span, 'the expression cannot be called as a function')
		return
	
	fnCallExpr.resolvedType = fnType.resolvedReturnType
	
	if len(fnCallExpr.args) < len(fnType.resolvedParamTypes) or \
		not fnType.cVarArgs and len(fnCallExpr.args) > len(fnType.resolvedParamTypes):
		logError(state, fnCallExpr.span, 
			'function called with wrong number of arguments (expected {}, found {})'.format(len(fnType.resolvedParamTypes), len(fnCallExpr.args)))
		return
	
	for (expected, arg) in zip(fnType.resolvedParamTypes, fnCallExpr.args):
		resolveValueExprType(state, scope, arg, expected)
		assignType = getValidAssignType(expected, arg.resolvedType)
		if assignType:
			arg.resolvedType = assignType
		else:
			logError(state, arg.span, 'expected type {}, found {}'.format(expected, arg.resolvedType))
	
	if fnType.cVarArgs and len(fnCallExpr.args) > len(fnType.resolvedParamTypes):
		for arg in fnCallExpr.args[len(fnType.resolvedParamTypes):]:
			resolveValueExprType(state, scope, arg)
			if arg.resolvedType and not arg.resolvedType.isPrimitiveType:
				logError(state, arg.span, 
					'type {} cannot be used as a C variadic argument'.format(arg.resolvedType))

def typeCheckReturn(state, scope, retExpr):
	fnScope = scope
	while type(scope) != FnDeclAST:
		scope = scope.parentScope
	
	if retExpr.expr:
		resolveValueExprType(state, scope, retExpr.expr, scope.resolvedSymbolType.resolvedReturnType)
		assignType = getValidAssignType(scope.resolvedSymbolType.resolvedReturnType, retExpr.expr.resolvedType)
		
		if assignType:
			retExpr.expr.resolvedType = assignType
		else:
			logError(state, retExpr.expr.span, 'invalid return type (expected {}, found {})'
				.format(scope.resolvedSymbolType.resolvedReturnType, retExpr.expr.resolvedType))

def typeCheckAsgn(state, scope, asgnExpr):
	resolveValueExprType(state, scope, asgnExpr.lvalue)
	resolveValueExprType(state, scope, asgnExpr.rvalue)
	
	assignType = getValidAssignType(asgnExpr.lvalue.resolvedType, asgnExpr.rvalue.resolvedType)
	if assignType:
		asgnExpr.rvalue.resolvedType = assignType
	else:
		logError(state, asgnExpr.rvalue.span, 'invalid types in assignment (expected {}, found {})'
			.format(asgnExpr.lvalue.resolvedType, asgnExpr.rvalue.resolvedType))
	
	if not lookupSymbol(state, scope, asgnExpr.lvalue, False).mut:
		logError(state, asgnExpr.lvalue.span, 'assignment target is not mutable')

def typeCheckWhile(state, scope, whileExpr):
	whileExpr.parentScope = scope
	scope = whileExpr
	
	resolveValueExprType(state, scope, whileExpr.expr)
	if whileExpr.expr.resolvedType != types.Bool:
		logError(state, whileExpr.expr.span, 
			'condition type must be Bool (found {})'.format(whileExpr.expr.resolvedType))
	
	typeCheckBlock(state, scope, whileExpr.block)

def getBlockType(block):
	if len(block.exprs) == 0:
		return types.Void
	
	lastExpr = block.exprs[-1]
	if not isinstance(lastExpr, ValueExprAST):
		return types.Void
	
	return lastExpr.resolvedType

def typeCheckIf(state, scope, ifExpr, expectedType):
	ifExpr.parentScope = scope
	scope = ifExpr
	
	resolveValueExprType(state, scope, ifExpr.expr)
	if ifExpr.expr.resolvedType != types.Bool:
		logError(state, ifExpr.expr.span, 
			'condition type must be Bool (found {})'.format(ifExpr.expr.resolvedType))
	
	typeCheckBlock(state, scope, ifExpr.ifBlock, expectedType)
	resolvedType = ifExpr.ifBlock.resolvedType
	
	if ifExpr.elseBlock:
		typeCheckBlock(state, scope, ifExpr.elseBlock, expectedType)
		elseResolvedType = ifExpr.elseBlock.resolvedType
		
		if ifExpr.ifBlock.doesReturn and ifExpr.elseBlock.doesReturn:
			resolvedType = expectedType if expectedType else types.Void
		elif ifExpr.ifBlock.doesReturn:
			resolvedType = ifExpr.elseBlock.resolvedType
		elif ifExpr.elseBlock.doesReturn:
			resolvedType = ifExpr.ifBlock.resolvedType
		else:
			superType = getValidAssignType(resolvedType, elseResolvedType)
			if not superType:
				superType = getValidAssignType(elseResolvedType, resolvedType)
				if not superType:
					superType = types.ResolvedOptionType(resolvedType, elseResolvedType)
			resolvedType = superType
	else:
		resolvedType = types.Void
	
	ifExpr.resolvedType = resolvedType

def typeCheckCoercion(state, scope, asExpr):
	resolveValueExprType(state, scope, asExpr.expr)
	resolveTypeRefType(state, scope, asExpr.typeRef)
	
	asExpr.resolvedType = asExpr.typeRef.resolvedType
	
	if not types.canCoerce(asExpr.expr.resolvedType, asExpr.typeRef.resolvedType):
		logError(state, asExpr.span, 'cannot coerce from {} to {}'
			.format(asExpr.expr.resolvedType, asExpr.typeRef.resolvedType))

def typeCheckBlock(state, scope, block, expectedType=None):
	block.parentScope = scope
	unreachableSpan = None
	block.doesReturn = False
	
	for (i, expr) in enumerate(block.exprs):
		if block.doesReturn:
			unreachableSpan = Span.merge(unreachableSpan, expr.span) if unreachableSpan else expr.span
		
		if type(expr) == LetAST:
			typeCheckLet(state, scope, expr)
			block.doesReturn = expr.doesReturn
		elif type(expr) == ReturnAST:
			typeCheckReturn(state, scope, expr)
			block.doesReturn = True
		elif type(expr) == AsgnAST:
			typeCheckAsgn(state, scope, expr)
			block.doesReturn = expr.doesReturn
		elif type(expr) == WhileAST:
			typeCheckWhile(state, scope, expr)
		elif isinstance(expr, ValueExprAST):
			resolveValueExprType(state, scope, expr, expectedType if i+1 == len(block.exprs) else None)
			block.doesReturn = expr.doesReturn
		else:
			assert 0
	
	if block.doesReturn or len(block.exprs) == 0:
		block.resolvedType = expectedType if expectedType else types.Void
	else:
		lastExpr = block.exprs[-1]
		if not isinstance(lastExpr, ValueExprAST):
			block.resolvedType = types.Void
		else:
			block.resolvedType = lastExpr.resolvedType
	
	if unreachableSpan:
		logWarning(state, unreachableSpan, 'unreachable code')

def typeCheckFnBody(state, fnDecl):
	if fnDecl.body == None:
		return
	
	typeCheckBlock(state, fnDecl, fnDecl.body, fnDecl.resolvedSymbolType.resolvedReturnType)
	
	if not fnDecl.body.doesReturn and getValidAssignType(
		fnDecl.resolvedSymbolType.resolvedReturnType, fnDecl.body.resolvedType, True) == None:
		logError(state, fnDecl.body.span, 'invalid return type (expected {}, found {})'
			.format(fnDecl.resolvedSymbolType.resolvedReturnType, fnDecl.body.resolvedType))

def mangleFnName(state, fnDecl):
	if fnDecl.cconv == CConv.C:
		fnDecl.mangledName = '_{}'.format(fnDecl.name)
	else:
		mangled = 'F{}{}'.format(len(fnDecl.name), fnDecl.name)
		parent = fnDecl.parentScope
		while parent:
			if type(parent) == ModAST:
				mangled = 'M{}{}{}'.format(len(parent.name), parent.name, mangled)
			elif type(parent) == FnDeclAST:
				mangled = 'F{}{}{}'.format(len(parent.name), parent.name, mangled)
			else:
				assert 0
			
			parent = parent.parentScope
		
		fnDecl.mangledName = mangled

def typeCheckMod(state, scope, mod):
	mod.parentScope = scope
	
	for decl in mod.importDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.staticDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		typeCheckFnSig(state, mod, decl)
		mangleFnName(state, decl)
	
	for decl in mod.modDecls:
		typeCheckMod(state, mod, decl)
	
	for decl in mod.fnDecls:
		typeCheckFnBody(state, decl)

class AnalyzerState:
	def __init__(self, mod):
		self.mod = mod
		self.failed = False

def analyze(mod):
	state = AnalyzerState(mod)
	
	typeCheckMod(state, None, mod)
	
	if state.failed:
		exit(1)
	
	return mod
