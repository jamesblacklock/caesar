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

class MIR:
	def __init__(self, span, hasValue=False):
		self.span = span
		self.hasValue = hasValue
		self.borrows = set()
		self.attrsInvoked = True
		self.analyzed = False
		self.contracts = None
	
	def setAnalyzed(self):
		pass
	
	def analyze(self, state, implicitType):
		return self
	
	def checkFlow(self, scope):
		assert 0
	
	def staticEval(self, state):
		return None
	
	def writeIR(self, state):
		assert 0
	
	def __str__(self):
		assert 0

def indent(str):
	return '    ' + '\n    '.join(str.split('\n'))

def printMIR(mod):
	for decl in mod.decls:
		if type(decl).__name__ in ('Mod', 'Impl'):
			printMIR(decl)
		elif type(decl).__name__ == 'FnDecl':
			print(str(decl))
