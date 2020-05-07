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