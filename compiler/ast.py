import sys
import ctypes
from enum import Enum

class StaticDataType(Enum):
	INT = 'INT'
	FLOAT = 'FLOAT'
	BYTES = 'BYTES'

class StaticData:
	def __init__(self, data, dataType, fType, label=None):
		self.label = label
		self.data = data
		self.dataType = dataType
		self.fType = fType
	
	def __intToBytes(self):
		if self.fType.byteSize == 1:
			t = ctypes.c_uint8
		elif self.fType.byteSize == 2:
			t = ctypes.c_uint16
		elif self.fType.byteSize == 4:
			t = ctypes.c_uint32
		elif self.fType.byteSize == 8:
			t = ctypes.c_uint64
		else:
			assert 0
		return [b for b in bytes(t(self.data))]
	
	def __floatToBytes(self):
		if self.fType.byteSize == 4:
			t = ctypes.c_float
		elif self.fType.byteSize == 8:
			t = ctypes.c_double
		else:
			assert 0
		return [b for b in bytes(t(self.data))]
	
	def toBytes(self):
		if self.dataType == StaticDataType.BYTES:
			return self.data
		elif self.dataType == StaticDataType.INT:
			return self._StaticData__intToBytes()
		elif self.dataType == StaticDataType.FLOAT:
			return self._StaticData__floatToBytes()
		else:
			assert 0

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
		self.hasValue = False
	
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
		self.dropFn = None
		self.type = None

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
		self.hasValue = True
	
	def staticEval(self, state):
		return None
		
