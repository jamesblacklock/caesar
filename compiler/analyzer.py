from .                   import ast
from .ast                import CConv, FnDeclAST, LetAST, FnCallAST, ReturnAST, IfAST, TypeRefAST, \
                                StrLitAST, IntLitAST, BoolLitAST, TupleLitAST, ValueRefAST, \
								InfixOpAST, FnCallAST, IfAST, CoercionAST, ModAST, ValueExprAST, \
								BlockAST, AsgnAST, WhileAST, DerefAST, IndexOpAST, VoidAST, \
								AddressAST, FloatLitAST, BreakAST, ContinueAST, InfixOp, LoopAST, \
                                CharLitAST, StructLitAST, FieldAccessAST, StructDeclAST
from .                   import types
from .types              import getValidAssignType
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
	symbolTok = path[0]
	
	while scope != None:
		if path[0].content in scope.symbolTable:
			symbol = scope.symbolTable[path[0].content]
			break
		scope = scope.parentScope
	
	if symbol != None:
		for tok in path[1:]:
			if state and type(symbol) != ModAST:
				logError(state, symbolTok.span, '`{}` is not a module'.format(symbol.name))
				return None
			
			symbolTok = tok
			if tok.content not in symbol.symbolTable:
				symbol = None
				break
			
			symbol = symbol.symbolTable[tok.content]
	
	if symbol and inTypePosition and type(symbol) == StructDeclAST:
		symbol = symbol.resolvedSymbolType
	
	if state:
		if inTypePosition:
			if symbol == None:
				logError(state, symbolTok.span, 'cannot resolve type `{}`'.format(symbolTok.content))
			elif not isinstance(symbol,  types.ResolvedType):
				logError(state, symbolTok.span, '`{}` is not a type'.format(symbolTok.content))
				symbol = None
		else:
			if symbol == None:
				logError(state, symbolTok.span, 'cannot resolve the symbol `{}`'.format(symbolTok.content))
			elif isinstance(symbol,  types.ResolvedType):
				logError(state, symbolTok.span, 'found a type reference where a value was expected')
				symbol = None
			elif type(symbol) == ModAST:
				logError(state, symbolTok.span, 'found a module name where a value was expected')
				symbol = None
	
	return symbol

def resolveValueExprType(state, scope, expr, implicitType=None):
	if type(expr) == StrLitAST:
		expr.resolvedType = types.ResolvedPtrType(types.Byte, 1)
	elif type(expr) == CharLitAST:
		expr.resolvedType = types.Char
	elif type(expr) == BoolLitAST:
		expr.resolvedType = types.Bool
	elif type(expr) == IntLitAST:
		typeCheckIntLit(state, scope, expr, implicitType)
	elif type(expr) == FloatLitAST:
		typeCheckFloatLit(state, scope, expr, implicitType)
	elif type(expr) == BlockAST:
		typeCheckBlock(state, scope, expr, implicitType)
	elif type(expr) == DerefAST:
		typeCheckDeref(state, scope, expr)
	elif type(expr) == AddressAST:
		typeCheckAddress(state, scope, expr, implicitType)
	elif type(expr) == TupleLitAST:
		raise RuntimeError('unimplemented!')
	elif type(expr) == ValueRefAST:
		typeCheckValueRef(state, scope, expr)
	elif type(expr) == InfixOpAST:
		typeCheckInfixOp(state, scope, expr, implicitType)
	elif type(expr) == FnCallAST:
		typeCheckFnCall(state, scope, expr)
	elif type(expr) == IndexOpAST:
		typeCheckIndex(state, scope, expr)
	elif type(expr) == IfAST:
		typeCheckIf(state, scope, expr, implicitType)
	elif type(expr) == CoercionAST:
		typeCheckCoercion(state, scope, expr)
	elif type(expr) == VoidAST:
		expr.resolvedType = types.Void
	elif type(expr) == StructLitAST:
		typeCheckStructLit(state, scope, expr)
	elif type(expr) == FieldAccessAST:
		typeCheckFieldAccess(state, scope, expr)
	else:
		assert 0

def resolveTypeRefType(state, scope, typeRef):
	resolvedType = lookupSymbol(state, scope, typeRef, True)
	
	if resolvedType == None:
		resolvedType = types.ResolvedType(typeRef.name, 0)
	
	if typeRef.indirectionLevel > 0:
		resolvedType = types.ResolvedPtrType(resolvedType, typeRef.indirectionLevel)
	
	typeRef.resolvedType = resolvedType

def typeCheckValueRef(state, scope, valueRef):
	symbol = lookupSymbol(state, scope, valueRef, False)
	valueRef.resolvedType = symbol.resolvedSymbolType if symbol else None

def typeCheckIntLit(state, scope, lit, implicitType):
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
	elif implicitType and (implicitType.isIntType or (implicitType == types.Byte and lit.base != 10)):
		lit.resolvedType = implicitType
	elif types.canAccommodate(types.Int32, lit.value):
		lit.resolvedType = types.Int32
	elif types.canAccommodate(types.Int64, lit.value) or lit.value < 0:
		lit.resolvedType = types.Int64
	else:
		lit.resolvedType = types.UInt64
	
	if not types.canAccommodate(lit.resolvedType, lit.value):
		logError(state, lit.span, 'integer value out of range for type {}'.format(lit.resolvedType))

def typeCheckFloatLit(state, scope, lit, implicitType):
	if lit.suffix == 'f32':
		lit.resolvedType = types.Float32
	elif lit.suffix == 'f64':
		lit.resolvedType = types.Float64
	elif implicitType and implicitType.isFloatType:
		lit.resolvedType = implicitType
	else:# elif types.canAccommodate(types.Float32, lit.value):
		lit.resolvedType = types.Float32
	# else:
	# 	lit.resolvedType = types.Float64
	
	# if not types.canAccommodate(lit.resolvedType, lit.value):
	# 	logError(state, lit.span, 'flaoting point value out of range for type {}'.format(lit.resolvedType))

def typeCheckAddress(state, scope, addr, implicitType):
	resolveValueExprType(state, scope, addr.expr, implicitType)
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

def typeCheckInfixOp(state, scope, infixOp, implicitType):
	opErr = lambda: \
		logError(state, infixOp.opSpan, 
			'invalid operand types for operator `{}` ({} and {})'
				.format(infixOp.op.desc(), infixOp.l.resolvedType, infixOp.r.resolvedType))
	
	lIndefinite = not types.hasDefiniteType(infixOp.l)
	rIndefinite = not types.hasDefiniteType(infixOp.r)
	if lIndefinite and rIndefinite:
		if type(infixOp.l) == IntLitAST and type(infixOp.r) == IntLitAST:
			if types.canAccommodate(types.Int32, infixOp.l.value) and \
				types.canAccommodate(types.Int32, infixOp.r.value):
				resolveValueExprType(state, scope, infixOp.r, types.Int32)
				resolveValueExprType(state, scope, infixOp.l, types.Int32)
			elif types.canAccommodate(types.Int64, infixOp.l.value) and \
				types.canAccommodate(types.Int64, infixOp.r.value):
				resolveValueExprType(state, scope, infixOp.r, types.Int64)
				resolveValueExprType(state, scope, infixOp.l, types.Int64)
			else:
				resolveValueExprType(state, scope, infixOp.r, types.UInt64)
				resolveValueExprType(state, scope, infixOp.l, types.UInt64)
		else:
			assert 0
	elif rIndefinite:
		resolveValueExprType(state, scope, infixOp.l, implicitType)
		if type(infixOp.r) == IntLitAST:
			if infixOp.l.resolvedType and infixOp.l.resolvedType.isPtrType:
				if infixOp.op in ast.PTR_INT_OPS:
					resolveValueExprType(state, scope, infixOp.r, types.ISize if infixOp.r.value < 0 else types.USize)
				else:
					opErr()
					return
			else:
				resolveValueExprType(state, scope, infixOp.r, infixOp.l.resolvedType)
		else:
			assert 0
	elif lIndefinite:
		resolveValueExprType(state, scope, infixOp.r, implicitType)
		if type(infixOp.l) == IntLitAST:
			if infixOp.r.resolvedType and infixOp.r.resolvedType.isPtrType:
				if infixOp.op in ast.PTR_INT_OPS:
					resolveValueExprType(state, scope, infixOp.l, types.ISize if infixOp.l.value < 0 else types.USize)
				else:
					opErr()
					return
			else:
				resolveValueExprType(state, scope, infixOp.l, infixOp.r.resolvedType)
		else:
			assert 0
	else:
		resolveValueExprType(state, scope, infixOp.l, implicitType)
		resolveValueExprType(state, scope, infixOp.r, implicitType)
	
	if not infixOp.l.resolvedType or not infixOp.r.resolvedType:
		return
	
	if infixOp.l.resolvedType == types.Byte and infixOp.r.resolvedType == types.Byte:
		if infixOp.op == InfixOp.EQ:
			infixOp.resolvedType = types.Bool
			return
	elif infixOp.l.resolvedType == types.Char and infixOp.r.resolvedType == types.Char:
		if infixOp.op == InfixOp.EQ:
			infixOp.resolvedType = types.Bool
			return
	elif infixOp.l.resolvedType.isPtrType and infixOp.r.resolvedType.isIntType:
		if infixOp.op in ast.PTR_INT_OPS and infixOp.r.resolvedType.byteSize == types.USize.byteSize:
			infixOp.resolvedType = infixOp.l.resolvedType
			return
	elif infixOp.l.resolvedType.isIntType and infixOp.r.resolvedType.isPtrType:
		if infixOp.op in ast.INT_PTR_OPS and infixOp.l.resolvedType.byteSize == types.USize.byteSize:
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
	
	resolvedReturnType = types.Void
	if fnDecl.returnType:
		resolveTypeRefType(state, fnDecl, fnDecl.returnType)
		resolvedReturnType = fnDecl.returnType.resolvedType
	
	for param in fnDecl.params:
		resolveTypeRefType(state, fnDecl, param.typeRef)
		param.resolvedSymbolType = param.typeRef.resolvedType
		if param.name in fnDecl.symbolTable:
			logError(state, param.span, 'duplicate parameter name')
		else:
			fnDecl.symbolTable[param.name] = param
	
	resolvedParamTypes = [param.typeRef.resolvedType for param in fnDecl.params]
	fnDecl.resolvedSymbolType = types.ResolvedFnType(resolvedParamTypes, fnDecl.cVarArgs, resolvedReturnType)
	
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
	
	if letExpr.name == '_':
		letExpr.noBinding = True
	else:
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
	
	if letExpr.resolvedSymbolType == types.Void:
		letExpr.noBinding = True

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
			'function called with wrong number of arguments (expected {}, found {})'
				.format(len(fnType.resolvedParamTypes), len(fnCallExpr.args)))
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
	resolveValueExprType(state, scope, asgnExpr.rvalue, asgnExpr.lvalue.resolvedType)
	
	assignType = getValidAssignType(asgnExpr.lvalue.resolvedType, asgnExpr.rvalue.resolvedType)
	if assignType:
		asgnExpr.rvalue.resolvedType = assignType
	else:
		logError(state, asgnExpr.rvalue.span, 'invalid types in assignment (expected {}, found {})'
			.format(asgnExpr.lvalue.resolvedType, asgnExpr.rvalue.resolvedType))
	
	symbol = asgnExpr.lvalue
	if type(symbol) == DerefAST:
		symbol = symbol.expr
	if type(symbol) != ValueRefAST:
		assert 0
	
	if not lookupSymbol(state, scope, symbol, False).mut:
		logError(state, symbol.span, 'assignment target is not mutable')

def typeCheckWhile(state, scope, whileExpr):
	whileExpr.parentScope = scope
	scope = whileExpr
	
	resolveValueExprType(state, scope, whileExpr.expr)
	if whileExpr.expr.resolvedType and whileExpr.expr.resolvedType != types.Bool:
		logError(state, whileExpr.expr.span, 
			'condition type must be Bool (found {})'.format(whileExpr.expr.resolvedType))
	
	typeCheckBlock(state, scope, whileExpr.block)

def typeCheckLoop(state, scope, loop):
	loop.parentScope = scope
	scope = loop
	typeCheckBlock(state, scope, loop.block)

def getBlockType(block):
	if len(block.exprs) == 0:
		return types.Void
	
	lastExpr = block.exprs[-1]
	if not isinstance(lastExpr, ValueExprAST):
		return types.Void
	
	return lastExpr.resolvedType

def typeCheckIf(state, scope, ifExpr, implicitType):
	ifExpr.parentScope = scope
	scope = ifExpr
	
	resolveValueExprType(state, scope, ifExpr.expr)
	if ifExpr.expr.resolvedType and ifExpr.expr.resolvedType != types.Bool:
		logError(state, ifExpr.expr.span, 
			'condition type must be Bool (found {})'.format(ifExpr.expr.resolvedType))
	
	typeCheckBlock(state, scope, ifExpr.ifBlock, implicitType)
	resolvedType = ifExpr.ifBlock.resolvedType
	
	if ifExpr.elseBlock:
		typeCheckBlock(state, scope, ifExpr.elseBlock, implicitType)
		elseResolvedType = ifExpr.elseBlock.resolvedType
		
		if ifExpr.ifBlock.doesReturn and ifExpr.elseBlock.doesReturn:
			resolvedType = implicitType if implicitType else types.Void
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

def typeCheckStructLit(state, scope, expr):
	resolvedType = lookupSymbol(state, scope, expr, True)
	
	if resolvedType == None:
		resolvedType = types.ResolvedType(expr.name, 0)
		return
	
	if not resolvedType.isStructType:
		logError(state, expr.nameTok.span, 'type `{}` is not a struct type'.format(expr.name))
		return
	
	fieldDict = resolvedType.fieldDict
	fieldNames = set()
	for field in expr.fields:
		if field.name in fieldNames:
			logError(state, field.nameTok.span, 'field `{}` was already initialized'.format(field.name))
		fieldNames.add(field.name)
		
		fieldType = None
		if field.name in fieldDict:
			fieldType = fieldDict[field.name].resolvedSymbolType
		else:
			logError(state, field.nameTok.span, 
				'struct `{}` has no field `{}`'.format(resolvedType.name, field.name))
		
		resolveValueExprType(state, scope, field.expr, fieldType)
		if field.expr.resolvedType and fieldType and \
			not types.getValidAssignType(fieldType, field.expr.resolvedType):
			logError(state, field.expr.span, 
				'expected type {}, found {}'.format(fieldType, field.expr.resolvedType))
	
	uninit = [field for field in fieldDict if field not in fieldNames]
	if len(uninit) > 0:
		fieldsStr = None
		if len(uninit) == 1:
			fieldsStr = uninit[0]
		elif len(uninit) == 2:
			fieldsStr = '{} and {}'.format(*uninit)
		elif len(uninit) < 5:
			fieldsStr = '{}, and {}'.format(', '.join(uninit[:-1]), uninit[-1])
		else:
			fieldsStr = '{}, and {} other fields'.format(', '.join(uninit[:3]), len(uninit) - 3)
		message = 'missing {} {} in initializer of `{}`'.format(
			'field' if len(uninit) == 1 else 'fields',
			fieldsStr,
			expr.name
		)
		logError(state, expr.nameTok.span, message)
	
	expr.resolvedType = resolvedType

def typeCheckFieldAccess(state, scope, expr):
	resolveValueExprType(state, scope, expr.expr)
	
	t = expr.expr.resolvedType
	if t == None:
		return
	
	fieldOffset = 0
	errSpan = expr.expr.span
	for tok in expr.path:
		if not t.isStructType:
			logError(state, errSpan, 'type `{}` has no fields'.format(t.name))
			return
		
		if tok.content not in t.fieldDict:
			logError(state, expr.span, 'type `{}` has no field `{}`'.format(t.name, tok.content))
			return
		
		field = t.fieldDict[tok.content]
		fieldOffset = field.offset
		t = field.resolvedSymbolType
		if t == None:
			return
	
	expr.fieldOffset = fieldOffset

def checkLoopCtl(state, scope, expr):
	while scope:
		if type(scope) == WhileAST or type(scope) == LoopAST:
			return
		scope = scope.parentScope
	
	logError(state, expr.span, '`{}` expression is not inside a loop'
		.format('break' if type(expr) == BreakAST else 'continue'))

def typeCheckBlock(state, scope, block, implicitType=None):
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
		elif type(expr) == LoopAST:
			typeCheckLoop(state, scope, expr)
		elif type(expr) == WhileAST:
			typeCheckWhile(state, scope, expr)
		elif type(expr) == BreakAST or type(expr) == ContinueAST:
			checkLoopCtl(state, scope, expr)
		elif isinstance(expr, ValueExprAST):
			resolveValueExprType(state, scope, expr, implicitType if i+1 == len(block.exprs) else None)
			block.doesReturn = expr.doesReturn
		else:
			assert 0
	
	if block.doesReturn or len(block.exprs) == 0:
		block.resolvedType = implicitType if implicitType else types.Void
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

def typeCheckStructDecl(state, scope, decl):
	fields = []
	offset = 0
	fieldNames = set()
	
	for field in decl.fields:
		if field.name in fieldNames:
			logError(state, field.nameTok.span, 'duplicate field declared in struct')
		fieldNames.add(field.name)
		
		resolveTypeRefType(state, scope, field.typeRef)
		
		align = types.getAlignment(field.typeRef.resolvedType)
		if offset % align > 0:
			offset += align - offset % align
		
		fields.append(types.Field(field.name, field.typeRef.resolvedType, offset))
		offset += field.typeRef.resolvedType.byteSize
	
	decl.resolvedSymbolType = types.ResolvedStructType(decl.name, fields)

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
	
	for decl in mod.structDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.structDecls:
		typeCheckStructDecl(state, mod, decl)
	
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
