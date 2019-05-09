from .parser             import CConv, FnDeclAST, LetAST, FnCallAST, ReturnAST, IfAST
from .types              import Type, Void
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

def expectTypeExists(state, typeRef):
	if typeRef.name not in state.mod.symbolTable:
		logError(state, typeRef.span, 'cannot resolve type `{}`'.format(typeRef.name))
	elif type(state.mod.symbolTable[typeRef.name]) != Type:
		logError(state, typeRef.span, '`{}` is not a type'.format(typeRef.name))

def typeCheckFnSig(state, fnDecl):
	if fnDecl.returnType == None:
		fnDecl.returnType = Void
	
	expectTypeExists(state, fnDecl.returnType)
	
	for param in fnDecl.params:
		expectTypeExists(state, param.type)
	
	if fnDecl.cVarArgs and fnDecl.cconv != CConv.C:
		logError(state, fnDecl.cVarArgsSpan, 
			'may not use C variadic parameter without the C calling convention')

def typeCheckLet(state, fnDecl, letExpr):
	determineValueExprType(state, letExpr.expr)
	expectTypeExists(state, letExpr.type)

def typeCheckFnCall(state, fnDecl, fnCallexpr):
	raise RuntimeError('unimplemented!')

def typeCheckReturn(state, fnDecl, retExpr):
	raise RuntimeError('unimplemented!')

def typeCheckIf(state, fnDecl, ifExpr):
	raise RuntimeError('unimplemented!')

def typeCheckFnBody(state, fnDecl):
	if fnDecl.body == None:
		return
	
	for expr in fnDecl.body:
		if type(expr) == LetAST:
			typeCheckLet(state, fnDecl, expr)
		elif type(expr) == FnCallAST:
			typeCheckFnCall(state, fnDecl, expr)
		elif type(expr) == ReturnAST:
			typeCheckReturn(state, fnDecl, expr)
		elif type(expr) == IfAST:
			typeCheckIf(state, fnDecl, expr)
		else:
			assert 0
	
	state.failed = True

class AnalyzerState:
	def __init__(self, mod):
		self.mod = mod
		self.failed = False

def analyze(mod):
	state = AnalyzerState(mod)
	
	for decl in mod.importDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.staticDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		invokeAttrs(state, decl)
	
	for decl in mod.fnDecls:
		typeCheckFnSig(state, decl)
	
	for decl in mod.fnDecls:
		typeCheckFnBody(state, decl)
	
	if state.failed:
		exit(1)
	
	return mod
