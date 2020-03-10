from .                   import ast
from .ast                import CConv, FnDeclAST, LetAST, FnCallAST, ReturnAST, IfAST, TypeRefAST, \
                                StrLitAST, IntLitAST, BoolLitAST, TupleLitAST, ValueRefAST, \
								InfixOpAST, FnCallAST, IfAST, CoercionAST, ModAST, ValueExprAST, \
								BlockAST, AsgnAST, WhileAST, DerefAST, IndexOpAST, VoidAST, \
								AddressAST, FloatLitAST, BreakAST, ContinueAST, InfixOp, LoopAST, \
                                CharLitAST, StructLitAST, FieldAccessAST, StructDeclAST, FnParamAST, \
                                SignAST, FieldDeclAST
from .                   import types
from .types              import getValidAssignType
from .err                import logError, logWarning
from .span               import Span
from .types              import BUILTIN_TYPES

def ffiAttr(state, decl, params, span):
	if len(params) != 1 or params[0].content != '"C"':
		logError(state, span, '"ffi" currently only supports the "C" convention')
		return
	if type(decl) != FnDeclAST:
		logError(state, span, '"ffi" attribute may only be applied to functions')
		return
	
	decl.cconv = CConv.C

def alignAttr(state, decl, params, span):
	align = params[0].content if len(params) == 1 else None
	if align != None:
		try:
			align = int(align)
		except ValueError:
			align = None
	
	if align == None:
		logError(state, span, '"align" takes a single integer parameter')
		return
	elif align < 1 or (align & (align - 1)) != 0:
		logError(state, span, 'alignment must be a power of 2 greater than or equal to 1')
		return
	elif type(decl) != FieldDeclAST:
		logError(state, span, 'the "align" attribute may only be applied to struct & field declarations')
		return
	
	decl.align = align

builtinAttrs = {
	'ffi': ffiAttr,
	'align': alignAttr
}

def invokeAttrs(state, decl):
	for attr in decl.attrs:
		if attr.name not in builtinAttrs:
			logError(state, attr.span, 'unrecognized attribute: "{}"'.format(attr.name))
			continue
		
		builtinAttrs[attr.name](state, decl, attr.args, attr.span)

def analyzeValueExpr(state, expr, implicitType=None):
	if type(expr) == StrLitAST:
		expr.resolvedType = types.ResolvedPtrType(types.Byte, 1)
	elif type(expr) == CharLitAST:
		expr.resolvedType = types.Char
	elif type(expr) == BoolLitAST:
		expr.resolvedType = types.Bool
	elif type(expr) == IntLitAST:
		analyzeIntLit(state, expr, implicitType)
	elif type(expr) == FloatLitAST:
		analyzeFloatLit(state, expr, implicitType)
	elif type(expr) == BlockAST:
		analyzeBlock(state, expr, implicitType)
	elif type(expr) == DerefAST:
		analyzeDeref(state, expr)
	elif type(expr) == SignAST:
		analyzeSign(state, expr)
	elif type(expr) == AddressAST:
		analyzeAddress(state, expr, implicitType)
	elif type(expr) == TupleLitAST:
		raise RuntimeError('unimplemented!')
	elif type(expr) == ValueRefAST:
		analyzeValueRef(state, expr)
	elif type(expr) == InfixOpAST:
		analyzeInfixOp(state, expr, implicitType)
	elif type(expr) == FnCallAST:
		analyzeFnCall(state, expr)
	elif type(expr) == IndexOpAST:
		analyzeIndex(state, expr)
	elif type(expr) == IfAST:
		analyzeIf(state, expr, implicitType)
	elif type(expr) == CoercionAST:
		analyzeCoercion(state, expr)
	elif type(expr) == VoidAST:
		expr.resolvedType = types.Void
	elif type(expr) == StructLitAST:
		analyzeStructLit(state, expr)
	elif type(expr) == FieldAccessAST:
		analyzeFieldAccess(state, expr)
	else:
		assert 0

def resolveTypeRef(state, typeRef):
	resolvedType = state.lookupSymbol(typeRef, True)
	
	if resolvedType == None:
		resolvedType = types.ResolvedType(typeRef.name, 0)
	
	if typeRef.indirectionLevel > 0:
		resolvedType = types.ResolvedPtrType(resolvedType, typeRef.indirectionLevel)
	
	typeRef.resolvedType = resolvedType

def analyzeValueRef(state, valueRef, noRef=False):
	valueRef.symbol = state.lookupSymbol(valueRef, False)
	if valueRef.symbol:
		valueRef.symbol.unused = False
		valueRef.resolvedType = valueRef.symbol.resolvedSymbolType
		
		if not noRef:
			state.refSymbol(valueRef)
	else:
		valueRef.resolvedType = None

def analyzeIntLit(state, lit, implicitType):
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

def analyzeFloatLit(state, lit, implicitType):
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

def analyzeAddress(state, addr, implicitType):
	analyzeValueExpr(state, addr.expr, implicitType)
	if addr.expr.resolvedType == None:
		return
	
	baseType = addr.expr.resolvedType
	indLevel = 1
	if baseType.isPtrType:
		baseType = baseType.baseType
		indLevel = baseType.indLevel + 1
	
	addr.resolvedType = types.ResolvedPtrType(baseType, indLevel)

def analyzeDeref(state, deref):
	analyzeValueExpr(state, deref.expr)
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

def analyzeSign(state, ast):
	analyzeValueExpr(state, ast.expr)
	ast.resolvedType = ast.expr.resolvedType
	if ast.expr.resolvedType == None:
		return
	elif not ast.expr.resolvedType.isSigned:
		logError(state, ast.expr.span, 'type "{}" has no sign'.format(ast.expr.resolvedType.name))

def analyzeInfixOp(state, infixOp, implicitType):
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
				analyzeValueExpr(state, infixOp.r, types.Int32)
				analyzeValueExpr(state, infixOp.l, types.Int32)
			elif types.canAccommodate(types.Int64, infixOp.l.value) and \
				types.canAccommodate(types.Int64, infixOp.r.value):
				analyzeValueExpr(state, infixOp.r, types.Int64)
				analyzeValueExpr(state, infixOp.l, types.Int64)
			else:
				analyzeValueExpr(state, infixOp.r, types.UInt64)
				analyzeValueExpr(state, infixOp.l, types.UInt64)
		else:
			assert 0
	elif rIndefinite:
		analyzeValueExpr(state, infixOp.l, implicitType)
		if type(infixOp.r) == IntLitAST:
			if infixOp.l.resolvedType and infixOp.l.resolvedType.isPtrType:
				if infixOp.op in ast.PTR_INT_OPS:
					analyzeValueExpr(state, infixOp.r, types.ISize if infixOp.r.value < 0 else types.USize)
				else:
					opErr()
					return
			else:
				analyzeValueExpr(state, infixOp.r, infixOp.l.resolvedType)
		else:
			assert 0
	elif lIndefinite:
		analyzeValueExpr(state, infixOp.r, implicitType)
		if type(infixOp.l) == IntLitAST:
			if infixOp.r.resolvedType and infixOp.r.resolvedType.isPtrType:
				if infixOp.op in ast.PTR_INT_OPS:
					analyzeValueExpr(state, infixOp.l, types.ISize if infixOp.l.value < 0 else types.USize)
				else:
					opErr()
					return
			else:
				analyzeValueExpr(state, infixOp.l, infixOp.r.resolvedType)
		else:
			assert 0
	else:
		analyzeValueExpr(state, infixOp.l, implicitType)
		analyzeValueExpr(state, infixOp.r, implicitType)
	
	if not infixOp.l.resolvedType or not infixOp.r.resolvedType:
		return
	
	if infixOp.l.resolvedType == types.Bool and infixOp.r.resolvedType == types.Bool:
		if infixOp.op == InfixOp.EQ:
			infixOp.resolvedType = types.Bool
			return
	elif infixOp.l.resolvedType == types.Byte and infixOp.r.resolvedType == types.Byte:
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

def analyzeIndex(state, expr):
	analyzeValueExpr(state, expr.expr)
	analyzeValueExpr(state, expr.index, types.ISize)
	
	if not expr.expr.resolvedType.isPtrType:
		logError(state, expr.expr.span, 
			'base of index expression must be an pointer type (found {})'.format(expr.expr.resolvedType))
	
	if not expr.index.resolvedType.isIntType:
		logError(state, expr.index.span, 'index must be an integer (found {})'.format(expr.index.resolvedType))
	
	indLevel = expr.expr.resolvedType.indirectionLevel
	baseType = expr.expr.resolvedType.baseType
	if indLevel == 1:
		expr.resolvedType = baseType
	else:
		expr.resolvedType = types.ResolvedPtrType(baseType, indLevel - 1)

def analyzeFnSig(state, fnDecl):
	resolvedReturnType = types.Void
	if fnDecl.returnType:
		resolveTypeRef(state, fnDecl.returnType)
		resolvedReturnType = fnDecl.returnType.resolvedType
	
	symbolNames = set()
	for param in fnDecl.params:
		resolveTypeRef(state, param.typeRef)
		param.resolvedSymbolType = param.typeRef.resolvedType
		if param.name in symbolNames:
			logError(state, param.span, 'duplicate parameter name')
		else:
			symbolNames.add(param.name)
	
	resolvedParamTypes = [param.typeRef.resolvedType for param in fnDecl.params]
	fnDecl.resolvedSymbolType = types.ResolvedFnType(resolvedParamTypes, fnDecl.cVarArgs, resolvedReturnType)
	
	if fnDecl.cVarArgs and fnDecl.cconv != CConv.C:
		logError(state, fnDecl.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')

def analyzeLet(state, letExpr):
	if letExpr.typeRef:
		resolveTypeRef(state, letExpr.typeRef)
		letExpr.resolvedSymbolType = letExpr.typeRef.resolvedType
		
		if letExpr.expr:
			analyzeValueExpr(state, letExpr.expr, letExpr.resolvedSymbolType)
			assignType = getValidAssignType(letExpr.resolvedSymbolType, letExpr.expr.resolvedType)
			if assignType:
				letExpr.expr.resolvedType = assignType
			else:
				logError(state, letExpr.expr.span, 'expected type {}, found {}'
					.format(letExpr.resolvedSymbolType, letExpr.expr.resolvedType))
	elif letExpr.expr:
		analyzeValueExpr(state, letExpr.expr)
		letExpr.resolvedSymbolType = letExpr.expr.resolvedType
	else:
		logError(state, letExpr.expr.span, 'cannot infer type of `{}`'.format(letExpr.name))
	
	if letExpr.name == '_':
		letExpr.noBinding = True
	else:
		state.declSymbol(letExpr)

def analyzeFnCall(state, fnCallExpr):
	analyzeValueExpr(state, fnCallExpr.expr)
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
		analyzeValueExpr(state, arg, expected)
		assignType = getValidAssignType(expected, arg.resolvedType)
		if assignType:
			arg.resolvedType = assignType
		else:
			logError(state, arg.span, 'expected type {}, found {}'.format(expected, arg.resolvedType))
	
	if fnType.cVarArgs and len(fnCallExpr.args) > len(fnType.resolvedParamTypes):
		for arg in fnCallExpr.args[len(fnType.resolvedParamTypes):]:
			analyzeValueExpr(state, arg)
			if arg.resolvedType and not arg.resolvedType.isPrimitiveType:
				logError(state, arg.span, 
					'type {} cannot be used as a C variadic argument'.format(arg.resolvedType))

def analyzeReturn(state, retExpr):
	scope = state.scope
	fnDecl = scope.fnDecl
	
	if retExpr.expr:
		analyzeValueExpr(state, retExpr.expr, fnDecl.resolvedSymbolType.resolvedReturnType)
		assignType = getValidAssignType(fnDecl.resolvedSymbolType.resolvedReturnType, retExpr.expr.resolvedType)
		
		if assignType:
			retExpr.expr.resolvedType = assignType
		else:
			logError(state, retExpr.expr.span, 'invalid return type (expected {}, found {})'
				.format(fnDecl.resolvedSymbolType.resolvedReturnType, retExpr.expr.resolvedType))

def analyzeAsgn(state, asgnExpr):
	if type(asgnExpr.lvalue) == ValueRefAST:
		analyzeValueRef(state, asgnExpr.lvalue, noRef=True)
	elif type(asgnExpr.lvalue) == FieldAccessAST:
		analyzeFieldAccess(state, asgnExpr.lvalue, noRef=True)
	else:
		analyzeValueExpr(state, asgnExpr.lvalue)
	analyzeValueExpr(state, asgnExpr.rvalue, asgnExpr.lvalue.resolvedType)
	
	assignType = getValidAssignType(asgnExpr.lvalue.resolvedType, asgnExpr.rvalue.resolvedType)
	if assignType:
		asgnExpr.rvalue.resolvedType = assignType
	else:
		logError(state, asgnExpr.rvalue.span, 'invalid types in assignment (expected {}, found {})'
			.format(asgnExpr.lvalue.resolvedType, asgnExpr.rvalue.resolvedType))
	
	if type(asgnExpr.lvalue) == ValueRefAST:
		if asgnExpr.lvalue.symbol != None:
			state.assignSymbol(asgnExpr.lvalue)
	elif type(asgnExpr.lvalue) == FieldAccessAST:
		if type(asgnExpr.lvalue.expr) == ValueRefAST and asgnExpr.lvalue.expr.symbol != None:
			state.assignSymbol(asgnExpr.lvalue.expr, asgnExpr.lvalue.field, asgnExpr.lvalue)
	elif type(asgnExpr.lvalue) == DerefAST:
		pass
	else:
		assert 0

def analyzeWhile(state, whileExpr):
	# the test needs to be considered in a new scope because it can be run 
	# multiple times and potentially assign to variables in an outer scope
	state.pushScope(whileExpr.block, isLoop=True)
	
	analyzeValueExpr(state, whileExpr.expr)
	if whileExpr.expr.resolvedType and whileExpr.expr.resolvedType != types.Bool:
		logError(state, whileExpr.expr.span, 
			'condition type must be Bool (found {})'.format(whileExpr.expr.resolvedType))
	
	whileExpr.block.resultUnused = True
	analyzeBlock(state, whileExpr.block, manageScope=False)
	state.popScope()

def analyzeLoop(state, loop):
	loop.block.resultUnused = True
	analyzeBlock(state, loop.block, isLoop=True)

def getBlockType(block):
	if len(block.exprs) == 0:
		return types.Void
	
	lastExpr = block.exprs[-1]
	if not isinstance(lastExpr, ValueExprAST):
		return types.Void
	
	return lastExpr.resolvedType

def analyzeIf(state, ifExpr, implicitType):
	analyzeValueExpr(state, ifExpr.expr)
	if ifExpr.expr.resolvedType and ifExpr.expr.resolvedType != types.Bool:
		logError(state, ifExpr.expr.span, 
			'condition type must be Bool (found {})'.format(ifExpr.expr.resolvedType))
	
	ifExpr.block.resultUnused = ifExpr.resultUnused
	analyzeBlock(state, ifExpr.block, implicitType, isIf=True)
	resolvedType = ifExpr.block.resolvedType
	
	ifExpr.elseBlock.resultUnused = ifExpr.resultUnused
	analyzeBlock(state, ifExpr.elseBlock, implicitType, isElse=True, ifExpr=ifExpr)
	elseResolvedType = ifExpr.elseBlock.resolvedType
	
	ifExpr.doesReturn = ifExpr.block.doesReturn and ifExpr.elseBlock.doesReturn
	
	if ifExpr.block.doesReturn and ifExpr.elseBlock.doesReturn:
		resolvedType = implicitType if implicitType else types.Void
	elif ifExpr.block.doesReturn:
		resolvedType = ifExpr.elseBlock.resolvedType
	elif ifExpr.elseBlock.doesReturn:
		resolvedType = ifExpr.block.resolvedType
	else:
		superType = getValidAssignType(resolvedType, elseResolvedType)
		if not superType:
			superType = getValidAssignType(elseResolvedType, resolvedType)
			if not superType:
				superType = types.ResolvedOptionType(resolvedType, elseResolvedType)
		resolvedType = superType
	
	ifExpr.resolvedType = resolvedType

def analyzeCoercion(state, asExpr):
	analyzeValueExpr(state, asExpr.expr)
	resolveTypeRef(state, asExpr.typeRef)
	
	asExpr.resolvedType = asExpr.typeRef.resolvedType
	
	if not types.canCoerce(asExpr.expr.resolvedType, asExpr.typeRef.resolvedType):
		logError(state, asExpr.span, 'cannot coerce from {} to {}'
			.format(asExpr.expr.resolvedType, asExpr.typeRef.resolvedType))

def analyzeStructLit(state, expr):
	resolvedType = state.lookupSymbol(expr, True)
	
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
		
		analyzeValueExpr(state, field.expr, fieldType)
		if field.expr.resolvedType and fieldType and \
			not types.getValidAssignType(fieldType, field.expr.resolvedType):
			logError(state, field.expr.span, 
				'expected type {}, found {}'.format(fieldType, field.expr.resolvedType))
	
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
	
	expr.resolvedType = resolvedType

def analyzeFieldAccess(state, expr, noRef=False):
	isRef = False
	if type(expr.expr) == ValueRefAST:
		isRef = not noRef
		analyzeValueRef(state, expr.expr, noRef=True)
	else:
		analyzeValueExpr(state, expr.expr)
	
	t = expr.expr.resolvedType
	if t == None:
		return
	
	fieldOffset = 0
	field = None
	errSpan = expr.expr.span
	for tok in expr.path:
		if not t.isStructType:
			logError(state, errSpan, 'type `{}` has no fields'.format(t.name))
			return
		
		if tok.content not in t.fieldDict:
			logError(state, expr.span, 'type `{}` has no field `{}`'.format(t.name, tok.content))
			return
		
		field = t.fieldDict[tok.content]
		fieldOffset += field.offset
		t = field.resolvedSymbolType
		if t == None:
			return
	
	expr.field = field
	expr.fieldOffset = fieldOffset
	expr.resolvedType = t
	
	if isRef:
		state.refSymbol(expr.expr, field, expr.span)

def analyzeLoopCtl(state, expr):
	if state.loopNest == 0:
		logError(state, expr.span, '`{}` expression is not inside a loop'
			.format('break' if type(expr) == BreakAST else 'continue'))
	
	symbolInfo = state.scope.symbolInfo
	scope = state.scope
	while True:
		for symbol in scope.symbolTable.values():
			if not symbolInfo[symbol].uninitialized:
				expr.dropSymbols.append(symbol)
		
		if scope.isLoop:
			break
		
		scope = scope.parent

def analyzeBlock(state, block, implicitType=None, \
	isLoop=False, isIf=False, isElse=False, ifExpr=None, fnDecl=None, manageScope=True):
	if manageScope:
		state.pushScope(block, isLoop=isLoop, isIf=isIf, isElse=isElse, ifExpr=ifExpr, fnDecl=fnDecl)
	
	unreachableSpan = None
	block.doesReturn = False
	
	for (i, expr) in enumerate(block.exprs):
		if block.doesReturn or block.doesBreak:
			unreachableSpan = Span.merge(unreachableSpan, expr.span) if unreachableSpan else expr.span
		
		if type(expr) == LetAST:
			analyzeLet(state, expr)
			block.doesReturn = expr.doesReturn
			block.doesBreak = expr.doesBreak
		elif type(expr) == ReturnAST:
			analyzeReturn(state, expr)
			block.doesReturn = True
		elif type(expr) == AsgnAST:
			analyzeAsgn(state, expr)
			block.doesReturn = expr.doesReturn
			block.doesBreak = expr.doesBreak
		elif type(expr) == LoopAST:
			analyzeLoop(state, expr)
		elif type(expr) == WhileAST:
			analyzeWhile(state, expr)
		elif type(expr) == BreakAST or type(expr) == ContinueAST:
			analyzeLoopCtl(state, expr)
			block.doesBreak = True
		elif isinstance(expr, ValueExprAST):
			lastExpr = i+1 == len(block.exprs)
			expr.resultUnused = block.resultUnused or not lastExpr
			analyzeValueExpr(state, expr, implicitType if lastExpr else None)
			block.doesReturn = expr.doesReturn
			block.doesBreak = expr.doesBreak
		else:
			assert 0
	
	if block.doesReturn or block.doesBreak or len(block.exprs) == 0:
		block.resolvedType = implicitType if implicitType else types.Void
	else:
		lastExpr = block.exprs[-1]
		if not isinstance(lastExpr, ValueExprAST):
			block.resolvedType = types.Void
		else:
			block.resolvedType = lastExpr.resolvedType
	
	if unreachableSpan:
		logWarning(state, unreachableSpan, 'unreachable code')
	
	if manageScope:
		state.popScope()

def analyzeFnBody(state, fnDecl):
	if fnDecl.body == None:
		return
	
	if fnDecl.resolvedSymbolType.resolvedReturnType == types.Void:
		fnDecl.body.resultUnused = True
	
	analyzeBlock(state, fnDecl.body, fnDecl.resolvedSymbolType.resolvedReturnType, fnDecl=fnDecl)
	
	if not fnDecl.body.doesReturn and getValidAssignType(
		fnDecl.resolvedSymbolType.resolvedReturnType, fnDecl.body.resolvedType, True) == None:
		logError(state, fnDecl.body.span, 'invalid return type (expected {}, found {})'
			.format(fnDecl.resolvedSymbolType.resolvedReturnType, fnDecl.body.resolvedType))

def analyzeStructDecl(state, decl):
	fields = []
	maxAlign = 0
	offset = 0
	fieldNames = set()
	
	for field in decl.fields:
		if field.name in fieldNames:
			logError(state, field.nameTok.span, 'duplicate field declared in struct')
		else:
			fieldNames.add(field.name)
		
		resolveTypeRef(state, field.typeRef)
		
		field.align = types.getAlignment(field.typeRef.resolvedType)
		maxAlign = max(maxAlign, field.align)
		invokeAttrs(state, field)
		
		if offset % field.align > 0:
			offset += field.align - offset % field.align
		
		fields.append(types.Field(field.name, field.typeRef.resolvedType, offset))
		offset += field.typeRef.resolvedType.byteSize
	
	offset += maxAlign - offset % maxAlign
	decl.resolvedSymbolType = types.ResolvedStructType(decl.name, maxAlign, offset, fields)

def mangleFnName(state, fnDecl):
	if fnDecl.cconv == CConv.C:
		fnDecl.mangledName = '_{}'.format(fnDecl.name)
	else:
		mangled = 'F{}{}'.format(len(fnDecl.name), fnDecl.name)
		scope = state.scope
		while scope:
			if scope.isMod:
				mangled = 'M{}{}{}'.format(len(scope.mod.name), scope.mod.name, mangled)
			elif scope.isFn:
				mangled = 'F{}{}{}'.format(len(scope.fnDecl.name), scope.fnDecl.name, mangled)
			else:
				assert 0
			
			scope = scope.parent
		
		fnDecl.mangledName = mangled

class FieldInfo:
	def __init__(self, field, initialized):
		self.field = field
		self.moved = False
		self.maybeMoved = False
		self.uninitialized = not initialized
		self.maybeUninitialized = False

class SymbolInfo:
	def __init__(self, symbol):
		self.symbol = symbol
		self.wasDeclared = False
		self.wasTouched = False
		self.lastUses = set()
		self.dropInIf = []
		self.dropInElse = []
		self.moved = False
		self.maybeMoved = False
		self.fieldInfo = {}
		
		def addFieldInfo(fields, expr):
			allInit = expr and type(expr) != StructLitAST
			fieldDict = expr.fieldDict if type(expr) == StructLitAST else {}
			for field in fields:
				self.fieldInfo[field] = FieldInfo(field, allInit or field.name in fieldDict)
				fieldExpr = fieldDict[field.name].expr if field.name in fieldDict else None
				if field.resolvedSymbolType.isStructType:
					addFieldInfo(field.resolvedSymbolType.fields, fieldExpr)
		
		if type(symbol) in (FnParamAST, FnDeclAST, ModAST, StructDeclAST):
			self.uninitialized = False
			self.maybeUninitialized = False
		elif type(symbol) == LetAST:
			self.uninitialized = symbol.expr == None
			self.maybeUninitialized = False
			if symbol.resolvedSymbolType.isStructType:
				addFieldInfo(symbol.resolvedSymbolType.fields, symbol.expr)
		else:
			assert 0
	
	def clone(self):
		info = SymbolInfo(self.symbol)
		info.wasTouched = self.wasTouched
		info.lastUses = self.lastUses
		info.dropInIf = self.dropInIf
		info.dropInElse = self.dropInElse
		info.moved = self.moved
		info.maybeMoved = self.maybeMoved
		info.uninitialized = self.uninitialized
		info.maybeUninitialized = self.maybeUninitialized
		
		return info

class Scope:
	def __init__(self, ast, parent, isLoop, isIf, ifBranchInfo, ifExpr, fnDecl):
		self.ast = ast
		self.parent = parent
		self.isLoop = isLoop
		self.isIf = isIf
		self.ifExpr = ifExpr
		self.isElse = ifBranchInfo != None
		self.ifBranchInfo = ifBranchInfo
		self.isFn = fnDecl != None
		self.fnDecl = fnDecl if fnDecl else parent.fnDecl if parent else None
		self.isMod = type(ast) == ModAST
		self.mod = ast if self.isMod else parent.mod
		self.symbolTable = {}
		self.symbolInfo = {}
		
		if self.parent:
			for info in self.parent.symbolInfo.values():
				info = info.clone()
				info.wasTouched = False
				self.symbolInfo[info.symbol] = info
		else:
			for builtin in BUILTIN_TYPES:
				self.symbolTable[builtin.name] = builtin
		
		if self.isFn:
			for symbol in fnDecl.params:
				self.symbolTable[symbol.name] = symbol
				info = SymbolInfo(symbol)
				info.wasDeclared = True
				self.symbolInfo[symbol] = info
		elif self.isMod:
			for decl in self.mod.decls:
				if decl.name in self.symbolTable:
					logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
				else:
					self.symbolTable[decl.name] = decl
					self.symbolInfo[decl] = SymbolInfo(decl)
					if type(decl) == ModAST:
						self.loadModSymbolInfo(decl)
	
	def loadModSymbolInfo(self, mod):
		for decl in mod.decls:
			self.symbolInfo[decl] = SymbolInfo(decl)
			if type(decl) == ModAST:
				self.loadModSymbolInfo(decl)
	
	def declSymbol(self, symbol):
		info = SymbolInfo(symbol)
		info.wasDeclared = True
		self.symbolInfo[symbol] = info
		self.symbolTable[symbol.name] = symbol
	
	def refSymbol(self, ref, field=None):
		info = self.symbolInfo[ref.symbol]
		info.wasTouched = True
		info.lastUses = set([ref])
		info.dropInIf = []
		info.dropInElse = []
		if field:
			info.fieldInfo[field].moved = not field.resolvedSymbolType.isCopyable
		else:
			info.moved = not ref.symbol.resolvedSymbolType.isCopyable
	
	def assignSymbol(self, ref, field=None):
		info = self.symbolInfo[ref.symbol]
		info.wasTouched = True
		info.lastUses = set([ref])
		info.dropInIf = []
		info.dropInElse = []
		info.uninitialized = False
		if field:
			info.fieldInfo[field].uninitialized = False
	
	def lookupSymbol(self, name):
		scope = self
		while scope != None:
			if name in scope.symbolTable:
				return scope.symbolTable[name]
			scope = scope.parent
		return None

def analyzeMod(state, mod):
	state.pushScope(mod)
	
	for decl in mod.importDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.staticDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.structDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.structDecls:
		analyzeStructDecl(state, decl)
	
	for decl in mod.fnDecls:
		analyzeFnSig(state, decl)
		mangleFnName(state, decl)
	
	for decl in mod.modDecls:
		analyzeMod(state, decl)
	
	for decl in mod.fnDecls:
		analyzeFnBody(state, decl)
	
	state.popScope()

class AnalyzerState:
	def __init__(self):
		self.ifBranchInfo = None
		self.scope = None
		self.loopNest = 0
		self.failed = False
		self.lastUses = []
	
	def pushScope(self, ast, isLoop=False, isIf=False, isElse=False, ifExpr=None, fnDecl=None):
		if self.loopNest > 0 or isLoop:
			self.loopNest += 1
		
		if isElse:
			assert self.ifBranchInfo != None
			ifBranchInfo = { info.symbol: info for info in self.ifBranchInfo.values() \
				if info.wasTouched and not info.wasDeclared }
		else:
			ifBranchInfo = None
		
		self.scope = Scope(ast, self.scope, isLoop, isIf, ifBranchInfo, ifExpr, fnDecl)
	
	def popScope(self):
		propagate = True
		symbolInfo = {}
		for info in self.scope.symbolInfo.values():
			if info.wasDeclared:
				for lastUse in info.lastUses:
					lastUse.lastUse = True
					# print(revealSpan(lastUse.span))
				for ifExpr in info.dropInIf:
					ifExpr.block.dropSymbols.append(info.symbol)
					# print(revealSpan(Span.cursor(ifExpr.block.span), 'drop "{}" here'.format(info.symbol.name)))
				for ifExpr in info.dropInElse:
					ifExpr.elseBlock.dropSymbols.append(info.symbol)
					# print(revealSpan(Span.cursor(ifExpr.elseBlock.span), 'drop "{}" here'.format(info.symbol.name)))
			elif info.wasTouched:
				symbolInfo[info.symbol] = info
		
		if self.scope.isMod:
			propagate = False
		if self.scope.isFn:
			propagate = False
			for symbol in self.scope.symbolTable.values():
				if type(symbol) == LetAST and symbol.unused:
					logWarning(self, symbol.span, 'unused symbol')
		elif self.scope.isIf:
			propagate = False
			self.ifBranchInfo = symbolInfo
		elif self.scope.isElse:
			symbols = set()
			symbols.update(symbolInfo.keys())
			symbols.update(self.scope.ifBranchInfo.keys())
			
			for symbol in symbols:
				if symbol in symbolInfo and symbol in self.scope.ifBranchInfo:
					branchInfo = self.scope.ifBranchInfo[symbol]
					info = symbolInfo[symbol]
					info.wasTouched = True
					info.lastUses.update(branchInfo.lastUses)
					info.maybeMoved = info.maybeMoved or \
						branchInfo.maybeMoved or (info.moved != branchInfo.moved)
					info.moved = info.maybeMoved
					info.maybeUninitialized = info.maybeUninitialized or \
						branchInfo.maybeUninitialized or (info.uninitialized != branchInfo.uninitialized)
					info.uninitialized = info.maybeUninitialized
				else:
					if symbol in symbolInfo:
						info = symbolInfo[symbol]
						info.dropInIf.append(self.scope.ifExpr)
					else:
						info = self.scope.ifBranchInfo[symbol].clone()
						info.dropInElse.append(self.scope.ifExpr)
						symbolInfo[symbol] = info
					
					parentInfo = self.scope.parent.symbolInfo[symbol]
					if parentInfo.moved != info.moved:
						info.moved = True
						info.maybeMoved = True
					if parentInfo.uninitialized != info.uninitialized:
						info.uninitialized = True
						info.maybeUninitialized = True
		
		if self.loopNest > 0:
			self.loopNest -= 1
		
		self.scope = self.scope.parent
		if propagate:
			for info in symbolInfo.values():
				if info.symbol in self.scope.symbolInfo:
					info = info.clone()
					info.wasDeclared = self.scope.symbolInfo[info.symbol].wasDeclared
					self.scope.symbolInfo[info.symbol] = info
	
	def declSymbol(self, symbol):
		self.scope.declSymbol(symbol)
	
	def refSymbol(self, ref, field=None, fieldSpan=None):
		info = self.scope.symbolInfo[ref.symbol]
		if info.uninitialized:
			maybeText = "may not have" if info.maybeUninitialized else "has not"
			logError(self, ref.span, '"{}" {} been initialized'.format(ref.name, maybeText))
			return
		elif info.moved:
			maybeText = "may have" if info.maybeMoved else "has"
			logError(self, ref.span, 'the value in "{}" {} been moved'.format(ref.name, maybeText))
			return
		elif field:
			fieldInfo = info.fieldInfo[field]
			if fieldInfo.uninitialized:
				maybeText = "may not have" if fieldInfo.maybeUninitialized else "has not"
				logError(self, fieldSpan, 'the field "{}" {} been initialized'.format(field.name, maybeText))
				return
			elif fieldInfo.moved:
				maybeText = "may have" if fieldInfo.maybeMoved else "has"
				logError(self, fieldSpan, 'the value in field "{}" {} been moved'.format(field.name, maybeText))
				return
		
		self.scope.refSymbol(ref, field)
	
	def assignSymbol(self, ref, field=None, fieldSpan=None):
		info = self.scope.symbolInfo[ref.symbol]
		if not ref.symbol.mut and not info.uninitialized:
			logError(self, ref.span, 'assignment target is not mutable')
			return
		
		self.scope.assignSymbol(ref, field)
	
	def lookupSymbol(self, ref, inTypePosition=False):
		symbolTok = ref.path[0]
		path = ref.path[1:]
		
		symbol = self.scope.lookupSymbol(symbolTok.content)
		
		if symbol != None:
			for tok in path:
				if type(symbol) != ModAST:
					logError(self, symbolTok.span, '`{}` is not a module'.format(symbol.name))
					return None
				
				symbolTok = tok
				if tok.content not in symbol.symbolTable:
					symbol = None
					break
				
				symbol = symbol.symbolTable[tok.content]
		
		if symbol and inTypePosition and type(symbol) == StructDeclAST:
			symbol = symbol.resolvedSymbolType
		
		if inTypePosition:
			if symbol == None:
				logError(self, symbolTok.span, 'cannot resolve type `{}`'.format(symbolTok.content))
			elif not isinstance(symbol,  types.ResolvedType):
				logError(self, symbolTok.span, '`{}` is not a type'.format(symbolTok.content))
				symbol = None
		else:
			if symbol == None:
				logError(self, symbolTok.span, 'cannot resolve the symbol `{}`'.format(symbolTok.content))
			elif isinstance(symbol,  types.ResolvedType):
				logError(self, symbolTok.span, 'found a type reference where a value was expected')
				symbol = None
			elif type(symbol) == ModAST:
				logError(self, symbolTok.span, 'found a module name where a value was expected')
				symbol = None
		
		return symbol
		
		

def analyze(mod):
	state = AnalyzerState()
	analyzeMod(state, mod)
	
	if state.failed:
		exit(1)
	
	return mod
