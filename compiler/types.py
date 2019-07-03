from enum import Enum
from .ast import IntLitAST

class ResolvedType:
	def __init__(self, name, byteSize, 
		isFnType=False, isPtrType=False, isStructType=False, 
		isIntType=False, isFloatType=False, isOptionType=False, 
		isVoidType=False, isPrimitiveType=False, isSigned=False):
		self.name = name
		self.byteSize = byteSize
		self.isPrimitiveType = isPrimitiveType
		self.isVoidType = isVoidType
		self.isFnType = isFnType
		self.isPtrType = isPtrType
		self.isStructType = isStructType
		self.isIntType = isIntType
		self.isFloatType = isFloatType
		self.isOptionType = isOptionType
		self.isSigned = isSigned
	
	def __str__(self):
		return self.name

class ResolvedOptionType(ResolvedType):
	def __init__(self, *resolvedTypes):
		name = '|'.join([t.name for t in resolvedTypes])
		byteSize = 0
		for t in resolvedTypes:
			if t.byteSize > byteSize:
				byteSize = t.byteSize
		
		super().__init__(name, byteSize, isOptionType=True)
		self.resolvedTypes = resolvedTypes

class ResolvedFnType(ResolvedType):
	def __init__(self, resolvedParamTypes, cVarArgs, resolvedReturnType):
		name = 'fn({}{}{}) -> {}'.format(
			', '.join([t.name for t in resolvedParamTypes]), 
			', ' if cVarArgs and len(resolvedParamTypes) > 0 else '',
			'...' if cVarArgs else '',
			resolvedReturnType.name)
		super().__init__(name, 0, isFnType=True)
		self.resolvedReturnType = resolvedReturnType
		self.resolvedParamTypes = resolvedParamTypes
		self.cVarArgs = cVarArgs

class Field:
	def __init__(self, name, symbolType, offset):
		self.name = name
		self.resolvedSymbolType = symbolType
		self.offset = offset

class ResolvedStructType(ResolvedType):
	def __init__(self, name, fields):
		byteSize = 0
		self.fieldDict = {}
		self.fields = fields
		
		if fields:
			lastField = fields[-1]
			byteSize = lastField.offset + lastField.resolvedSymbolType.byteSize
			
			for field in fields:
				self.fieldDict[field.name] = field
		
		super().__init__(name, byteSize, isStructType=True)

PLATFORM_WORD_SIZE = 8

class ResolvedPtrType(ResolvedType):
	def __init__(self, baseType, indirectionLevel):
		name = baseType.name + ('&' * indirectionLevel)
		super().__init__(name, PLATFORM_WORD_SIZE, isPtrType=True, isPrimitiveType=True)
		self.baseType = baseType
		self.indirectionLevel = indirectionLevel

Void    = ResolvedType('Void',    0, isVoidType=True)
Bool    = ResolvedType('Bool',    1, isPrimitiveType=True)
Byte    = ResolvedType('Byte',    1, isPrimitiveType=True)
Int8    = ResolvedType('Int8',    1, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt8   = ResolvedType('UInt8',   1, isPrimitiveType=True, isIntType=True)
Int16   = ResolvedType('Int16',   2, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt16  = ResolvedType('UInt16',  2, isPrimitiveType=True, isIntType=True)
Int32   = ResolvedType('Int32',   4, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt32  = ResolvedType('UInt32',  4, isPrimitiveType=True, isIntType=True)
Char    = ResolvedType('Char',    4, isPrimitiveType=True)
Int64   = ResolvedType('Int64',   8, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt64  = ResolvedType('UInt64',  8, isPrimitiveType=True, isIntType=True)
ISize   = ResolvedType('ISize',   PLATFORM_WORD_SIZE, isPrimitiveType=True, isIntType=True, isSigned=True)
USize   = ResolvedType('USize',   PLATFORM_WORD_SIZE, isPrimitiveType=True, isIntType=True)
Float32 = ResolvedType('Float32', 4, isPrimitiveType=True, isFloatType=True)
Float64 = ResolvedType('Float64', 8, isPrimitiveType=True, isFloatType=True)

BUILTIN_TYPES = [
	Void,
	Bool,
	Byte,
	Int8,
	UInt8,
	Int16,
	UInt16,
	Int32,
	UInt32,
	Char,
	Int64,
	UInt64,
	ISize,
	USize,
	Float32,
	Float64
]

I8_MAX  = 127
I16_MAX = 32767
I32_MAX = 2147483647
I64_MAX = 9223372036854775807
ISZ_MAX = I64_MAX
I8_MIN  = -I8_MAX  - 1
I16_MIN = -I16_MAX - 1
I32_MIN = -I32_MAX - 1
I64_MIN = -I64_MAX - 1
ISZ_MIN = -ISZ_MAX - 1
U8_MAX  = 255
U16_MAX = 65535
U32_MAX = 4294967295
U64_MAX = 18446744073709551615
USZ_MAX = U64_MAX
I8_RNG  = range(I8_MIN, I8_MAX+1)
I16_RNG = range(I16_MIN, I16_MAX+1)
I32_RNG = range(I32_MIN, I32_MAX+1)
I64_RNG = range(I64_MIN, I64_MAX+1)
ISZ_RNG = range(ISZ_MIN, ISZ_MAX+1)
U8_RNG  = range(0, U8_MAX+1)
U16_RNG = range(0, U16_MAX+1)
U32_RNG = range(0, U32_MAX+1)
U64_RNG = range(0, U64_MAX+1)
USZ_RNG = range(0, USZ_MAX+1)

def canAccommodate(type, intValue):
	if type.byteSize == 1 and type.isSigned:
		return intValue in I8_RNG
	elif type.byteSize == 2 and type.isSigned:
		return intValue in I16_RNG
	elif type.byteSize == 4 and type.isSigned:
		return intValue in I32_RNG
	elif type.byteSize == 8 and type.isSigned:
		return intValue in I64_RNG
	elif type.byteSize == 1 and not type.isSigned:
		return intValue in U8_RNG
	elif type.byteSize == 2 and not type.isSigned:
		return intValue in U16_RNG
	elif type.byteSize == 4 and not type.isSigned:
		return intValue in U32_RNG
	elif type.byteSize == 8 and not type.isSigned:
		return intValue in U64_RNG
	else:
		assert 0

def getValidAssignType(expectedType, foundType, allowVoidCercion=False):
	if expectedType == None:
		return foundType
	elif foundType == None:
		return expectedType
	elif expectedType == Void and allowVoidCercion:
		return Void
	elif type(expectedType) == ResolvedPtrType and type(foundType) == ResolvedPtrType and \
		expectedType.baseType == foundType.baseType and \
		expectedType.indirectionLevel == foundType.indirectionLevel:
		return expectedType
	elif expectedType.isOptionType:
		if foundType.isOptionType and len(expectedType.resolvedTypes) == len(foundType.resolvedTypes):
			for t in expectedType.resolvedTypes:
				if t not in foundType.resolvedTypes:
					return None
			return expectedType
		else:
			return foundType in expectedType.resolvedTypes
	elif expectedType.isIntType and foundType.isIntType and \
		expectedType.byteSize == foundType.byteSize and expectedType.isSigned == foundType.isSigned:
		return expectedType
	elif expectedType is foundType:
		return expectedType
	else:
		return None

def canCoerce(fromType, toType):
	if fromType and toType and fromType.isPrimitiveType and toType.isPrimitiveType:
		return True
	elif toType == Void:
		return True
	else:
		return False

def hasDefiniteType(ast):
	if type(ast) == IntLitAST and (ast.suffix == None) and \
		(ast.value >= I32_MIN or ast.value <= I64_MAX):
		return False
	
	return True

def getAlignment(t):
	if t.byteSize == 0:
		return 1
	elif t.isPrimitiveType:
		return t.byteSize
	elif t.isStructType:
		assert len(t.fields) > 0
		return getAlignment(t.fields[0].resolvedSymbolType)
	else:
		assert 0