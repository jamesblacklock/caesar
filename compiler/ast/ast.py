class Contract:
	def __init__(self, symbol, enumType, variants, indLevel):
		self.symbol = symbol
		self.enumType = enumType
		self.variants = variants
		self.indLevel = indLevel
	
	def inverted(self):
		variants = [v for v in self.enumType.variants if v not in self.variants]
		return Contract(self.symbol, self.enumType, variants, self.indLevel)
	
	def intersect(self, other):
		variants = [v for v in self.variants if v in other.variants]
		return Contract(self.symbol, self.enumType, variants, self.indLevel)

class Name:
	def __init__(self, content, span):
		self.content = content
		self.span = span
	
	@staticmethod
	def fromTok(tok):
		return Name(tok.content, tok.span)

class AST:
	def __init__(self, span, hasValue=False, hasBlockValue=False):
		self.span = span
		self.hasValue = hasValue
		self.hasBlockValue = hasBlockValue
		self.attrs = []
		self.analyzed = False
		self.attrsInvoked = False
	
	def setAnalyzed(self):
		self.analyzed = True
	
	def analyze(self, state, implicitType):
		assert 0

class Attr(AST):
	def __init__(self, name, args, span):
		super().__init__(span)
		self.name = name
		self.args = args

class Symbol(AST):
	def __init__(self, name, span, doccomment=None, extern=False):
		super().__init__(span)
		self.nameSpan = name.span if name else span
		self.name = name.content if name else None
		self.unused = True
		self.symbolTable = None
		self.doccomment = doccomment
		self.extern = extern
		self.pub = False

class ValueSymbol(Symbol):
	def __init__(self, name, typeRef, span, doccomment=None, extern=False):
		super().__init__(name, span, doccomment, extern)
		self.typeRef = typeRef
