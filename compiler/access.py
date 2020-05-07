class AccessType(Enum):
	INDEX = 'INDEX'
	FIELD = 'FIELD'
	DEREF = 'DEREF'

class AccessOp(ValueExpr):
	def __init__(self, accessType, span):
		super().__init__(span)
		self.accessType = accessType

class DerefOp(AccessOp):
	def __init__(self, span):
		super().__init__(AccessType.DEREF, span)
	
	def pretty(self, output, indent=0):
		output.write('^', indent)

class IndexOp(AccessOp):
	def __init__(self, expr, span):
		super().__init__(AccessType.INDEX, span)
		self.expr = expr
	
	def pretty(self, output, indent=0):
		output.write('[', indent)
		self.expr.pretty(output)
		output.write(']')

class FieldOp(AccessOp):
	def __init__(self, path, span):
		super().__init__(AccessType.FIELD, span)
		self.path = path
		self.offset = None
		self.nameTok = path[-1]
		self.name = self.nameTok.content
	
	def pretty(self, output, indent=0):
		path = '.'.join(tok.content for tok in self.path)
		output.write('.', indent)
		output.write(path)

class Access(ValueExpr):
	def __init__(self, expr):
		super().__init__(expr.span)
		self.expr = expr
		self.ops = []
		self.field = None
		self.write = False
		self.copy = False
	
	def analyze(acc, state, implicitType):
		acc.expr = state.analyzeNode(acc.expr)
		if acc.expr.type == None and (not acc.write or acc.ops):
			return
		
		block = None
		if type(acc.expr) == valueref.ValueRef:
			symbol = acc.expr.symbol
			if symbol == None:
				return
		else:
			symbol, block = createTempBlock(acc.expr)
		
		# initial assignment may occur before the type has been determined, e.g.:
		#   let x; x = 5
		assert symbol.type or not acc.ops
		
		t = acc.expr.type
		field = None
		derefCount = 0
		for op in acc.ops:
			if type(op) == FieldOp:
				acc.analyzeFieldAccess(state, derefCount, field, op)
				field = op.field
				derefCount = 0
			elif type(op) == DerefOp:
				acc.analyzeDerefAccess(state, derefCount, field, op)
				derefCount += 1
			elif type(op) == IndexOp:
				acc.analyzeIndexAccess(state, derefCount, field, op)
				field = op.field
				derefCount = 0
			else:
				assert 0
			t = op.type
		
		if symbol and not acc.write:
			state.scope.readSymbol(acc, field)
		
		if implicitType and canPromote(t, implicitType):
			acc.expr = Coercion(acc.expr, implicitType)
		
		acc.type = t
	
	def writeIR(ast, state):
		assert ast.write == False # writes are handled differently, by Asgn.writeIR
		if type(ast.expr) == valueref.ValueRef and len(ast.ops) == 0:
			ast.expr.writeIR(state, ast.copy)
		else:
			assert 0