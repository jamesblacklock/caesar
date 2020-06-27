from ..ast.ast import ValueSymbol
from ..types import typesMatch, Void
from ..log  import logError
from ..ast      import attrs
from ..mir.mir import TypeModifiers

class StaticDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, doccomment, extern, mut, expr, span):
		super().__init__(nameTok, typeRef, span, doccomment, extern)
		self.mangledName = None
		self.mut = mut
		self.exprAST = expr
		self.staticValue = None
		self.type = None
		self.typeModifiers = TypeModifiers()
		self.contracts = None
		self.expr = None
		
	def analyzeSig(self, state, isConst=False):
		if self.typeRef:
			self.type = state.resolveTypeRef(self.typeRef)
		
		if self.name == '_':
			logError(state, self.exprAST.span, '`_` is not a valid symbol name')
		elif not isConst:
			self.mangledName = state.mangleName(self)
	
	def analyze(self, state):
		implicitType = None
		if self.type:
			implicitType = self.type
		
		self.expr = state.analyzeNode(self.exprAST, implicitType)
		if self.expr == None or self.expr.type == None:
			return
		
		if False and self.expr.type and self.expr.type.isCompositeType:
			assert 0
			if self.expr.typeModifiers == None:
				self.expr.typeModifiers = TypeModifiers()
			if len(self.expr.typeModifiers.uninitFields) > 0:
				logError(state, self.exprAST.span, 'global declarations must initialize all fields of composite types')
			
			self.typeModifiers = self.expr.typeModifiers.clone()
		else:
			self.typeModifiers.uninit = False
		
		# tryPromote???
		
		if self.type:
			if not typesMatch(self.type, self.expr.type):
				logError(state, self.exprAST.span, 'expected type {}, found {}'
					.format(self.type, self.expr.type))
		else:
			self.type = self.expr.type
		
		self.staticValue = self.expr.staticEval(state)
		if self.staticValue == None:
			logError(state, self.exprAST.span, 'expression cannot be statically evaluated')
		else:
			self.staticValue.label = self.mangledName
	
	def __str__(self, storageClass='static'):
		return '{} {}: {} = {}'.format(
			storageClass, self.name, self.type.name, str(self.mirExpr))

class ConstDecl(StaticDecl):
	def __init__(self, nameTok, typeRef, doccomment, expr, span):
		super().__init__(nameTok, typeRef, doccomment, False, False, expr, span)
		
	def analyzeSig(self, state):
		super().analyzeSig(state, isConst=True)
	
	def __str__(self):
		return super().__str__('const')
