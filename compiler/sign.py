from .ast import ValueExpr
from .ir  import FNeg, Neg

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