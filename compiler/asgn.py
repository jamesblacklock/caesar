from .ast       import AST
from .          import valueref, letdecl, block, deref, access
from .structlit import StructLit
from .field     import Index, Field
from .ifexpr    import If
from .types     import typesMatch
from .ir        import Swap, DerefW, FieldW, DerefFieldW, Fix, IPTR
from .scope     import ScopeType
from .log       import logError

class Asgn(AST):
	def __init__(self, lvalue, rvalue, span, temp=False):
		super().__init__(span)
		self.lvalue = lvalue
		self.rvalue = rvalue
	
	def analyze(asgn, state, ignoredImplicitType):
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
