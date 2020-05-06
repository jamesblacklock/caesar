from .ast      import ValueExpr
from .         import valueref, block, letdecl, asgn, field, deref, types
from .ir       import Addr
from .types    import PtrType

class Address(ValueExpr):
	def __init__(self, expr, span):
		super().__init__(span)
		self.expr = expr
	
	def lower(self, state):
		if type(self.expr) == valueref.ValueRef:
			return self
		elif type(self.expr) in (field.Index, field.Field, deref.Deref):
			assert 0
		
		temp = letdecl.LetDecl(None, None, False, None, self.span, temp=True)
		tempLValue = valueref.ValueRef(None, self.span, temp=True)
		tempLValue.symbol = temp
		tempAsgn = asgn.Asgn(tempLValue, self.expr, self.span, temp=True)
		tempAsgn.lowered = True
		
		self.expr = valueref.ValueRef(None, self.span, temp=True)
		self.expr.symbol = temp
		
		b = block.Block(block.BlockInfo([temp, tempAsgn, self], None))
		b.lowered = True
		return b
	
	def analyze(self, state, implicitType):
		baseType = None
		if implicitType and implicitType.isPtrType:
			baseType = implicitType.baseType
		
		self.expr = state.analyzeNode(self.expr, baseType)
		if self.expr.type == None:
			return
		
		if self.expr.type.isPtrType:
			self.type = PtrType(self.expr.type.baseType, self.expr.type.indLevel + 1)
		else:
			self.type = PtrType(self.expr.type, 1)
		
		state.scope.addrSymbol(self)
	
	def writeIR(ast, state):
		assert type(ast.expr) == valueref.ValueRef
		offset = state.localOffset(ast.expr.symbol)
		state.appendInstr(Addr(ast, offset))
	
	def pretty(self, output, indent=0):
		output.write('&', indent)
		self.expr.pretty(output)