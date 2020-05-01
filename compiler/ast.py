import sys

class TypeModifiers:
	def __init__(self, uninit=True):
		self.uninit = uninit
		self.uninitFields = set()
	
	def clone(self):
		modifiers = TypeModifiers(self.uninit)
		modifiers.uninitFields = { f for f in self.uninitFields }
		return modifiers

class ASTPrinter:
	def __init__(self):
		# self.output = StringIO()
		self.prefix = ''
	
	def write(self, s, indent=0):
		sys.stdout.write('    ' * indent)
		sys.stdout.write(self.prefix)
		self.prefix = ''
		sys.stdout.write(s)
		# self.output.write(s)
	
	def addPrefix(self, s):
		self.prefix = self.prefix + s
	
	# def getvalue(self):
	# 	return self.output.getvalue()

class AST:
	def __init__(self, span):
		self.attrs = None
		self.attrsInvoked = False
		self.span = span
	
	def analyze(self, state, implicitType):
		assert 0
	
	def lower(self, state):
		return self
	
	def writeIR(self, state):
		assert 0
	
	def pretty(self, output, indent=0):
		assert 0

class Symbol(AST):
	def __init__(self, nameTok, span, doccomment=None, extern=False):
		super().__init__(span)
		self.nameTok = nameTok
		self.name = nameTok.content if nameTok else None
		self.unused = True
		self.symbolTable = None
		self.doccomment = doccomment
		self.extern = extern

class TypeSymbol(Symbol):
	def __init__(self, nameTok, span, doccomment):
		super().__init__(nameTok, span, doccomment)
		self.declaredType = None

class ValueSymbol(Symbol):
	def __init__(self, nameTok, typeRef, span, doccomment=None, extern=False):
		super().__init__(nameTok, span, doccomment, extern)
		self.typeRef = typeRef
		self.type = None
		self.typeModifiers = TypeModifiers()

class FnParam(ValueSymbol):
	def __init__(self, nameTok, typeRef, span):
		super().__init__(nameTok, typeRef, span)
		self.dropFn = None
		self.dropBlock = None
	
	def analyze(param, state, implicitType):
		param.dropBlock = state.scope.fnDecl.body
		state.scope.declSymbol(param)
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.typeRef.pretty(output)

class CVarArgsParam(AST):
	def __init__(self, span):
		super().__init__(span)

class StaticDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, doccomment, extern, mut, expr, span):
		super().__init__(nameTok, typeRef, span, doccomment, extern)
		self.mangledName = None
		self.mut = mut
		self.expr = expr

class ValueExpr(AST):
	def __init__(self, span):
		super().__init__(span)
		self.type = None
		self.typeModifiers = None
		self.isConst = False
		self.constBytes = None

class Address(ValueExpr):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def writeIR(ast, state):
		assert type(ast.expr) == ValueRef
		offset = state.localOffset(ast.expr.symbol)
		state.appendInstr(Addr(ast, offset))

class Sign(ValueExpr):
	def __init__(self, expr, negate, span):
		super().__init__(span)
		self.expr = expr
		self.negate = negate

	def analyze(ast, state):
		ast.expr = state.analyzeNode(ast.expr)
		ast.type = ast.expr.type
		if ast.expr.type == None:
			return
		elif not ast.expr.type.isSigned:
			logError(state, ast.expr.span, 'type `{}` has no sign'.format(ast.expr.type.name))
	
	def writeIR(ast, state):
		ast.expr.writeIR(state)
		if ast.negate:
			if ast.type.isFloatType:
				state.appendInstr(FNeg(ast))
			else:
				state.appendInstr(Neg(ast))
