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
		
	def analyzeSig(decl, state, isConst=False):
		attrs.invokeAttrs(state, decl)
		
		if decl.typeRef:
			decl.type = state.resolveTypeRef(decl.typeRef)
		
		if decl.name == '_':
			logError(state, decl.expr.span, '`_` is not a valid symbol name')
		elif not isConst:
			decl.mangledName = state.mangleName(decl)
	
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
		
		decl.staticValue = decl.expr.staticEval(state)
		if decl.staticValue == None:
			logError(state, decl.expr.span, 'expression cannot be statically evaluated')
		else:
			decl.staticValue.label = decl.mangledName
	
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
		
	def analyzeSig(decl, state):
		super().analyzeSig(state, isConst=True)
	
	def pretty(self, output, indent=0):
		self._StaticDecl__pretty(output, indent, 'const')
