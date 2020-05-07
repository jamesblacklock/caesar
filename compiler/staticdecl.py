from .ast import ValueSymbol
from .types import getValidAssignType
from .log   import logError
from .      import attrs

class ConstDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, doccomment, expr, span):
		super().__init__(nameTok, typeRef, span, doccomment)
		self.expr = expr
		
	def analyzeSig(decl, state):
		attrs.invokeAttrs(state, decl)
		
		if decl.typeRef:
			decl.type = state.resolveTypeRef(decl.typeRef)
	
	def analyze(decl, state, implicitType):
		implicitType = None
		if decl.type:
			implicitType = decl.type
		
		decl.expr = state.analyzeNode(decl.expr, implicitType)
		if decl.expr.type and decl.expr.type.isCompositeType:
			if len(decl.expr.typeModifiers.uninitFields) > 0:
				logError(state, decl.expr.span, 'global declarations must initialize all fields of composite types')
			
			decl.typeModifiers.uninit = decl.expr.typeModifiers.clone()
		else:
			decl.typeModifiers.uninit = False
		
		if decl.type:
			assignType = getValidAssignType(decl.type, decl.expr.type)
			if assignType:
				decl.expr.type = assignType
			else:
				logError(state, decl.expr.span, 'expected type {}, found {}'
					.format(decl.type, decl.expr.type))
		else:
			decl.type = decl.expr.type
		
		if decl.name == '_':
			logError(state, decl.expr.span, '`_` is not a valid mod-level binding')
		
		if not decl.expr.isConst:
			logError(state, decl.expr.span, 
				'initializer for `{}` cannot be statically evaluated'.format(decl.name))
	
	def pretty(self, output, indent=0):
		output.write('const ' + self.name, indent)
		if self.typeRef:
			output.write(': ')
			self.typeRef.pretty(output)
		output.write(' = ')
		self.expr.pretty(output)

class StaticDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, doccomment, extern, mut, expr, span):
		super().__init__(nameTok, typeRef, span, doccomment, extern)
		self.mangledName = None
		self.mut = mut
		self.expr = expr
		
	def analyzeSig(decl, state):
		attrs.invokeAttrs(state, decl)
		
		if decl.typeRef:
			decl.type = state.resolveTypeRef(decl.typeRef)
	
	

# def resolveStaticDecl(state, decl):
# 	decl.bytes = resolveConstExpr(state, decl.expr)

# def resolveConstExpr(state, expr, resolvedType=None):
# 	if resolvedType == None:
# 		resolvedType = expr.type
	
# 	if type(expr) == IntLit:
# 		if resolvedType.byteSize == 1:
# 			t = ctypes.c_uint8
# 		elif resolvedType.byteSize == 2:
# 			t = ctypes.c_uint16
# 		elif resolvedType.byteSize == 4:
# 			t = ctypes.c_uint32
# 		elif resolvedType.byteSize == 8:
# 			t = ctypes.c_uint64
# 		else:
# 			assert 0
# 		return [b for b in bytes(t(expr.value))]
# 	elif type(expr) == StructLit:
# 		structBytes = [0 for _ in range(0, expr.type.byteSize)]
# 		for f in expr.type.fields:
# 			if f.name not in expr.fieldDict:
# 				continue
# 			fieldBytes = resolveConstExpr(state, expr.fieldDict[f.name].expr)
# 			end = f.offset + len(fieldBytes)
# 			structBytes[f.offset : end] = fieldBytes
# 		return structBytes
# 	elif type(expr) == Coercion:
# 		return resolveConstExpr(state, expr.expr, resolvedType)
# 	else:
# 		assert 0