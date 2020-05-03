from .ast   import ValueExpr
from .      import valueref
from .      import letdecl
from .      import asgn
from .      import block
from .scope import ScopeType
from .log   import logError
from .      import ir

class Deref(ValueExpr):
	def __init__(self, expr, count, span):
		super().__init__(span)
		self.expr = expr
		self.count = count
		self.write = False
	
	def lower(deref, state):
		if type(deref.expr) == valueref.ValueRef:
			return deref
		
		tempSymbol = letdecl.LetDecl(None, None, False, None, deref.span, temp=True)
		
		tempLValue = valueref.ValueRef(None, deref.span, temp=True)
		tempLValue.symbol = tempSymbol
		tempAsgn = asgn.Asgn(tempLValue, deref.expr, deref.span, temp=True)
		tempAsgn.lowered = True
		tempAsgn.dropBlock = block.Block(block.BlockInfo([], None))
		tempAsgn.dropBlock.lowered = True
		
		deref.expr = valueref.ValueRef(None, deref.span, temp=True)
		deref.expr.symbol = tempSymbol
		
		return block.Block(block.BlockInfo([tempSymbol, tempAsgn, deref], None), ScopeType.BLOCK)
	
	def analyze(deref, state, implicitType):
		deref.expr = state.analyzeNode(deref.expr)
		if deref.expr.type == None:
			return
		
		if not deref.expr.type.isPtrType:
			logError(state, deref.expr.span, 'cannot dereference non-pointer type `{}`'.format(deref.expr.type))
			return
		elif deref.count > deref.expr.type.indLevel:
			baseType = deref.expr.type.baseType if deref.expr.type.isPtrType else deref.expr.type
			logError(state, deref.expr.span, 
				'dereferenced too many times (max: {}; found: {})'.format(deref.expr.type.indLevel, deref.count))
			return
		
		deref.type = deref.expr.type.typeAfterDeref(deref.count)
	
	def writeIR(ast, state):
		ast.expr.writeIR(state)
		for i in range(0, ast.count):
			if i+1 < ast.count:
				fType = IPTR
			else:
				fType = ir.FundamentalType.fromResolvedType(ast.type)
			
			state.appendInstr(ir.Deref(ast, fType))
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write('^' * self.count)
		