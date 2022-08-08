from .symbol    import ValueSymbol
from ..types    import typesMatch
from ..log      import logError
from ..mir.mir  import TypeModifiers
from ..mir.flow import CFGBuilder

class Static(ValueSymbol):
	def __init__(self, ast, isConst):
		super().__init__(ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.extern = False
		self.mangledName = None
		self.mut = ast.mut
		self.staticValue = None
		self.isConst = isConst
		self.isStatic = not isConst
		self.mir = None
		self.analyzed = False
		self.typeModifiers = TypeModifiers()
		self.contracts = {}
	
	def checkSig(self, state):
		self.extern = self.ast.extern
		if self.ast.typeRef:
			self.type = self.ast.typeRef.resolveSig(state)
		if not self.isConst:
			self.mangledName = state.mangleName(self)
	
	def analyze(self, state, deps):
		if self.analyzed:
			return
		
		if self in deps:
			# logError(state, self.nameSpan, 'circular dependency detected')
			return
		
		deps.push(self)
		
		flow = CFGBuilder(state, None, state.mod, self.span)
		flow.beginScope(self.span)
		self.mir = flow.analyzeNode(self.ast.expr, self.type)
		flow.endScope()
		
		if self.mir == None or self.mir.type == None:
			assert flow.failed
			state.failed = True
			return
		
		# if False and self.expr.type and self.expr.type.isCompositeType:
		# 	assert 0
		# 	if self.expr.typeModifiers == None:
		# 		self.expr.typeModifiers = TypeModifiers()
		# 	if len(self.expr.typeModifiers.uninitFields) > 0:
		# 		logError(state, self.exprAST.span, 'global declarations must initialize all fields of composite types')
			
		# 	self.typeModifiers = self.expr.typeModifiers.clone()
		# else:
		# 	self.typeModifiers.uninit = False
		
		# tryPromote???
		
		if self.type:
			if not typesMatch(self.type, self.mir.type):
				logError(state, self.mir.span, 'expected type {}, found {}'.format(self.type, self.mir.type))
		else:
			self.type = self.mir.type
		
		self.staticValue = flow.staticEval(self.mir.symbol)
		if self.staticValue == None:
			logError(state, self.mir.span, 'expression cannot be statically evaluated')
		else:
			self.staticValue.label = self.mangledName
		
		self.analyzed = True
	
	def __str__(self):
		storageClass = 'const' if self.isConst else 'static'
		return '{} {}: {} = {}'.format(storageClass, self.name, self.type.name, str(self.mir))
