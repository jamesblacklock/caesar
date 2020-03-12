from enum import Enum
from .ast import IntLitAST

class ResolvedType:
	def __init__(self, name, byteSize, 
		isFnType=False, isPtrType=False, isStructType=False, 
		isIntType=False, isFloatType=False, isOptionType=False, 
		isVoidType=False, isPrimitiveType=False, isSigned=False,
		isArrayType=False, isTupleType=False, isCompositeType=False):
		self.name = name
		self.byteSize = byteSize
		self.isPrimitiveType = isPrimitiveType
		self.isCopyable = isPrimitiveType or isFnType
		self.isVoidType = isVoidType
		self.isFnType = isFnType
		self.isPtrType = isPtrType
		self.isStructType = isStructType
		self.isIntType = isIntType
		self.isFloatType = isFloatType
		self.isOptionType = isOptionType
		self.isSigned = isSigned
		self.isArrayType = isArrayType
		self.isTupleType = isTupleType
		self.isCompositeType = isCompositeType
	
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
		super().__init__(name, PLATFORM_WORD_SIZE, isFnType=True, isPtrType=True)
		self.resolvedReturnType = resolvedReturnType
		self.resolvedParamTypes = resolvedParamTypes
		self.cVarArgs = cVarArgs

class Field:
	def __init__(self, name, symbolType, offset):
		self.name = name
		self.resolvedSymbolType = symbolType
		self.offset = offset

class ResolvedStructType(ResolvedType):
	def __init__(self, name, align, byteSize, fields):
		super().__init__(name, byteSize, 
			isStructType=True, isCompositeType=True, isVoidType=(byteSize == 0))
		
		self.align = align
		self.byteSize = byteSize
		self.fields = fields
		self.fieldDict = {}
		
		if len(fields) > 0:
			for field in fields:
				self.fieldDict[field.name] = field

PLATFORM_WORD_SIZE = 8

class ResolvedPtrType(ResolvedType):
	def __init__(self, baseType, indirectionLevel):
		name = baseType.name + ('&' * indirectionLevel)
		super().__init__(name, PLATFORM_WORD_SIZE, isPtrType=True, isPrimitiveType=True)
		self.baseType = baseType
		self.indirectionLevel = indirectionLevel

class ArrayFields:
	def __init__(self, elementType, count):
		self.count = count
		self.elementType = elementType
	
	def __getitem__(self, index):
		if index >= self.count:
			raise IndexError()
		return Field(None, self.elementType, index * getAlignedSize(self.elementType))
	
	def __len__(self):
		return self.count

class ResolvedArrayType(ResolvedType):
	def __init__(self, baseType, count):
		name = '[{} * {}]'.format(baseType.name, count)
		elementAlign = getAlignment(baseType)
		byteSize = getAlignedSize(baseType) * count
		super().__init__(name, byteSize, 
			isArrayType=True, isCompositeType=True, isVoidType=(byteSize == 0))
		self.baseType = baseType
		self.count = count
		self.fields = ArrayFields(baseType, count)
		self.align = elementAlign

class ResolvedTupleType(ResolvedType):
	def __init__(self, align, byteSize, fields):
		name = '({})'.format(', '.join(f.resolvedSymbolType.name for f in fields))
		super().__init__(name, byteSize, 
			isTupleType=True, isCompositeType=True, isVoidType=(byteSize == 0))
		self.align = align
		self.fields = fields

Void    = ResolvedType('void',    0, isVoidType=True)
Bool    = ResolvedType('bool',    1, isPrimitiveType=True)
Byte    = ResolvedType('byte',    1, isPrimitiveType=True)
Int8    = ResolvedType('int8',    1, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt8   = ResolvedType('uint8',   1, isPrimitiveType=True, isIntType=True)
Int16   = ResolvedType('int16',   2, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt16  = ResolvedType('uint16',  2, isPrimitiveType=True, isIntType=True)
Int32   = ResolvedType('int',     4, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt32  = ResolvedType('uint',    4, isPrimitiveType=True, isIntType=True)
Char    = ResolvedType('char',    4, isPrimitiveType=True)
Int64   = ResolvedType('int64',   8, isPrimitiveType=True, isIntType=True, isSigned=True)
UInt64  = ResolvedType('uint64',  8, isPrimitiveType=True, isIntType=True)
ISize   = ResolvedType('isize',   PLATFORM_WORD_SIZE, isPrimitiveType=True, isIntType=True, isSigned=True)
USize   = ResolvedType('usize',   PLATFORM_WORD_SIZE, isPrimitiveType=True, isIntType=True)
Float32 = ResolvedType('float32', 4, isPrimitiveType=True, isFloatType=True, isSigned=True)
Float64 = ResolvedType('float64', 8, isPrimitiveType=True, isFloatType=True, isSigned=True)

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
	if expectedType is foundType:
		return expectedType
	elif expectedType == None:
		return foundType
	elif foundType == None:
		return expectedType
	elif expectedType == Void and allowVoidCercion:
		return Void
	elif expectedType.isPtrType and foundType.isPtrType and \
		getValidAssignType(expectedType.baseType, foundType.baseType) != None and \
		expectedType.indirectionLevel == foundType.indirectionLevel:
		return expectedType
	elif expectedType.isOptionType:
		if foundType.isOptionType and len(expectedType.resolvedTypes) == len(foundType.resolvedTypes):
			for t in expectedType.resolvedTypes:
				if t not in foundType.resolvedTypes:
					return None
			return expectedType
		elif foundType in expectedType.resolvedTypes:
			return expectedType
	elif expectedType.isIntType and foundType.isIntType and \
		expectedType.byteSize == foundType.byteSize and expectedType.isSigned == foundType.isSigned:
		return expectedType
	elif expectedType.isTupleType or foundType.isTupleType:
		return getValidTupleAssignType(expectedType, foundType)
	elif expectedType.isArrayType or foundType.isArrayType:
		return getValidArrayAssignType(expectedType, foundType)
	elif expectedType.isStructType and foundType.isStructType:
		return getValidStructAssignType(expectedType, foundType)
	else:
		return None

def getValidStructAssignType(expectedType, foundType):
	if expectedType.byteSize != foundType.byteSize:
		return None
	if expectedType.align != foundType.align:
		return None
	
	expectedFields = expectedType.fields
	foundFields    =    foundType.fields
	if len(expectedFields) != len(foundFields):
		return None
	
	for (expected, found) in zip(expectedFields, foundFields):
		if getValidAssignType(expected.resolvedSymbolType, found.resolvedSymbolType) == None:
			return None
		if expected.offset != found.offset:
			return None
		if expected.name != found.name:
			return None
	
	return expectedType

def getValidArrayAssignType(expectedType, foundType):
	if expectedType.isArrayType and foundType.isArrayType:
		if getValidAssignType(expectedType.baseType, foundType.baseType) == None:
			return None
		if expectedType.count != foundType.count:
			return None
		return expectedType
	
	if not (expectedType.isCompositeType and foundType.isCompositeType):
		return None
	
	return getValidTupleAssignType(expectedType, foundType)

def getValidTupleAssignType(expectedType, foundType):
	if expectedType.byteSize != foundType.byteSize:
		return None
	if getAlignment(expectedType) != getAlignment(foundType):
		return None
	
	expectedFields = expectedType.fields if expectedType.isCompositeType else [Field(None, expectedType, 0)]
	foundFields    =    foundType.fields if    foundType.isCompositeType else [Field(None,    foundType, 0)]
	if len(expectedFields) != len(foundFields):
		return None
	
	for (expected, found) in zip(expectedFields, foundFields):
		if getValidAssignType(expected.resolvedSymbolType, found.resolvedSymbolType) == None:
			return None
		if expected.offset != found.offset:
			return None
	
	return expectedType

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
	if t.isVoidType:
		return 0
	elif t.isPrimitiveType:
		return t.byteSize
	elif t.isStructType or t.isArrayType or t.isTupleType:
		return t.align
	else:
		assert 0

def getAlignedSize(t):
	align = getAlignment(t)
	byteSize = t.byteSize
	if t.byteSize % align > 0:
		byteSize += t.byteSize - t.byteSize % align
	
	return byteSize