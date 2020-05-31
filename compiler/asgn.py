from .ast       import AST
from .          import valueref, letdecl, block, deref, access
from .structlit import StructLit
from .field     import Index, Field
from .ifexpr    import If
from .types     import typesMatch
from .ir        import Swap, DerefW, FieldW, DerefFieldW, Fix, IPTR
from .token     import TokenType
from .scope     import ScopeType
from .log       import logError

class Asgn(AST):
	def __init__(self, lvalue, rvalue, opTok, span, temp=False):
		super().__init__(span)
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.opTok = opTok
		self.dropBlock = None
	
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
