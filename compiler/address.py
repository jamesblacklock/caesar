from .ast      import ValueExpr
from .         import valueref, block, letdecl, asgn, field, deref, types
from .ir       import Addr
from .types    import PtrType

class Address(ValueExpr):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
		self.lowered = False
		self.symbol = None
	
	def lower(self, state):
		if self.lowered:
			return self
		else:
			self.lowered = True
		
		if type(self.expr) != valueref.ValueRef:
			assert 0
		# elif type(self.expr) in (field.Index, field.Field, deref.Deref):
			# assert 0
		
		assert state.scope.dropBlock
		
		(temp, tempAsgn, tempRef) = letdecl.createTempTriple(self)
		
		b = block.Block([temp, tempAsgn, tempRef], None)
		b.lowered = True
		return b
	
	def analyze(self, state, implicitType):
		baseType = None
		if implicitType and implicitType.isPtrType:
			baseType = implicitType.baseType
		
		self.expr.addr = True
		self.expr = state.analyzeNode(self.expr, baseType)
		# self.expr.dropBlock = state.scope.dropBlock
		if self.expr.type == None:
			return
		
		if self.expr.type.isPtrType:
			self.type = PtrType(self.expr.type.baseType, self.expr.type.indLevel + 1)
		else:
			self.type = PtrType(self.expr.type, 1)
		
		self.borrows = {self}
		self.symbol = self.expr.symbol
		state.scope.addrSymbol(self)
	
	def writeIR(ast, state):
		assert type(ast.expr) == valueref.ValueRef
		offset = state.localOffset(ast.expr.symbol)
		state.appendInstr(Addr(ast, offset))
	
	def pretty(self, output, indent=0):
		output.write('&', indent)
		self.expr.pretty(output)