from .ast      import ValueExpr
from .         import fndecl, letdecl, staticdecl, block, asgn
from .access   import SymbolAccess
from .coercion import Coercion
from .types    import canPromote
from .ir       import IPTR, Dup, Raise, Global, Deref

class ValueRef(ValueExpr):
	def __init__(self, path, span):
		super().__init__(span)
		self.path = path
	
	def analyze(valueRef, state, implicitType):
		return SymbolAccess.analyzeSymbolAccess(state, valueRef, implicitType)
	
	def pretty(self, output, indent=0):
		closeParen = False
		if self.copy:
			output.addPrefix('$copy(')
			closeParen = True
		elif not (self.write or self.fieldAccess or self.addr) and \
			type(self.symbol) in (staticdecl.StaticDecl, letdecl.LetDecl, letdecl.FnParam):
			output.addPrefix('$move(')
			closeParen = True
		
		if self.path:
			path = '::'.join(tok.content for tok in self.path)
			output.write(path, indent)
		else:
			output.write(self.name, indent)
		
		if closeParen:
			output.write(')')
