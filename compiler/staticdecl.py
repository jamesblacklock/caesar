from .ast import ValueSymbol
from .types import getValidAssignType
from .log   import logError
from .      import attrs

class StaticDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, doccomment, extern, mut, expr, span):
		super().__init__(nameTok, typeRef, span, doccomment, extern)
		self.mangledName = None
		self.mut = mut
		self.expr = expr
		self.staticValue = None
		
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
		
		decl.staticValue = decl.expr.staticEval(state)
	
	def __pretty(self, output, indent, s):
		output.write('{} {}'.format(s, self.name), indent)
		if self.typeRef:
			output.write(': ')
			self.typeRef.pretty(output)
		output.write(' = ')
		self.expr.pretty(output)
	
	def pretty(self, output, indent=0):
		self._StaticDecl__pretty(output, indent, 'static')

class ConstDecl(StaticDecl):
	def __init__(self, nameTok, typeRef, doccomment, expr, span):
		super().__init__(nameTok, typeRef, doccomment, False, False, expr, span)
	
	def pretty(self, output, indent=0):
		self._StaticDecl__pretty(output, indent, 'const')
	
	

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







# class StructLit(ValueExpr):
		# if isConstExpr:
		# 	self.bytes = [0 for _ in range(0, self.type.byteSize)]
		# 	for (name, init) in fieldInits.items():
		# 		offset = self.type.fieldDict[name].offset
		# 		end = offset + len(init.bytes)
		# 		self.bytes[offset : end] = init.bytes