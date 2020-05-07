import sys
from .log import logError

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
	
	def lower(self, state):
		return self
	
	def analyze(self, state, implicitType):
		assert 0
	
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

class ValueExpr(AST):
	def __init__(self, span):
		super().__init__(span)
		self.type = None
		self.typeModifiers = None
		self.borrows = None
	
	def staticEval(self, state):
		logError(state, self.span, 'expression cannot be statically evaluated')
		return None
		
