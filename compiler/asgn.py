from .ast       import AST
from .          import block, deref, access
from .structlit import StructLit
from .ifexpr    import If
from .token     import TokenType

class Asgn(AST):
	def __init__(self, lvalue, rvalue, opTok, span, temp=False):
		super().__init__(span)
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.opTok = opTok
	
	def analyze(asgn, state, ignoredImplicitType):
		from .infix import InfixOp, InfixOps
		
		if asgn.opTok.type != TokenType.ASGN:
			if asgn.opTok.type == TokenType.TIMESASGN:
				infixOp = InfixOps.TIMES
			elif asgn.opTok.type == TokenType.DIVASGN:
				infixOp = InfixOps.DIV
			elif asgn.opTok.type == TokenType.MODULOASGN:
				infixOp = InfixOps.MODULO
			elif asgn.opTok.type == TokenType.PLUSASGN:
				infixOp = InfixOps.PLUS
			elif asgn.opTok.type == TokenType.MINUSASGN:
				infixOp = InfixOps.MINUS
			else:
				assert 0
			
			asgn.rvalue = InfixOp(asgn.lvalue, asgn.rvalue, infixOp, asgn.opTok, asgn.span)
		
		return access.SymbolAccess.analyzeSymbolAccess(state, asgn)
	
	def pretty(self, output, indent=0):
		self.lvalue.pretty(output, indent)
		if self.lvalue.deref:
			output.write('^')
		output.write(' = ')
		if type(self.rvalue) not in (block.Block, If, StructLit):
			indent = 0
		elif type(self.rvalue) in (If, StructLit):
			output.write('\n')
			indent += 1
		
		self.rvalue.pretty(output, indent)
