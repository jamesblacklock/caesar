from enum import Enum
from .    import primitive
from .ast import TypeModifiers

PLATFORM_WORD_SIZE = 8

class Type:
	def __init__(self, name, byteSize, align, 
		isFnType=False, isPtrType=False, isStructType=False, 
		isIntType=False, isIntLikeType=False, isFloatType=False, isOptionType=False, 
		isPrimitiveType=False, isSigned=False, isArrayType=False, isOwnedType=False, 
		isTupleType=False, isCompositeType=False, isResolved=True):
		self.name = name
		self.byteSize = byteSize
		self.align = align
		self.isResolved = isResolved
		self.isPrimitiveType = isPrimitiveType
		self.isCopyable = isFnType or isPrimitiveType
		self.isVoidType = byteSize == 0
		self.isFnType = isFnType
		self.isPtrType = isPtrType
		self.isStructType = isStructType
		self.isIntType = isIntType
		self.isIntLikeType = isIntType or isIntLikeType
		self.isFloatType = isFloatType
		self.isOptionType = isOptionType
		self.isSigned = isSigned
		self.isArrayType = isArrayType
		self.isTupleType = isTupleType
		self.isCompositeType = isCompositeType
		self.isOwnedType = isOwnedType
		self.dropFn = None
	
	def __str__(self):
		return self.name

class OwnedType(Type):
	def __init__(self, baseType, acquire, release, acquireSpan, releaseSpan):
		name = 'owned({}, {}) {}'.format(acquire.name, release.name, baseType.name)
		super().__init__(name, baseType.byteSize, baseType.align, isOwnedType=True)
		self.baseType = baseType
		self.acquire = acquire
		self.release = release
		self.acquireSpan = acquireSpan
		self.releaseSpan = releaseSpan

class PrimitiveType(Type):
	def __init__(self, name, byteSize, isIntType=False, 
		isIntLikeType=False, isFloatType=False, isSigned=False):
		super().__init__(name, byteSize, byteSize, 
			isPrimitiveType=True, isIntLikeType=isIntLikeType, isIntType=isIntType, 
			isFloatType=isFloatType, isSigned=isSigned)

class OptionType(Type):
	def __init__(self, *types):
		align = max(t.align for t in types)
		name = '|'.join(t.name for t in types)
		byteSize = max(t.byteSize for t in types)
		super().__init__(name, byteSize, align, isOptionType=True)
		self.types = types

class FnType(Type):
	def __init__(self, unsafe, params, returnType, cVarArgs, cconv):
		name = '{}fn({}{}{}) -> {}'.format(
			'unsafe ' if unsafe else '',
			', '.join([t.type.name for t in params]), 
			', ' if cVarArgs and len(params) > 0 else '',
			'...' if cVarArgs else '',
			returnType.name)
		super().__init__(name, PLATFORM_WORD_SIZE, PLATFORM_WORD_SIZE, 
			isFnType=True, isPtrType=True)
		self.unsafe = unsafe
		self.returnType = returnType
		self.returnTypeModifiers = TypeModifiers(False)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cconv = cconv

class FieldInfo:
	def __init__(self, name, symbolType, offset, isUnionField=False):
		self.name = name
		self.type = symbolType
		self.offset = offset
		self.isUnionField = isUnionField

class StructType(Type):
	def __init__(self, name, dropFn, align, byteSize, fields):
		super().__init__(name, byteSize, align, 
			isStructType=True, isCompositeType=True)
		
		self.name = name if name else '<anonymous struct>'
		self.anon = name == None
		self.dropFn = dropFn
		self.fields = fields
		self.fieldDict = {}
		
		if len(fields) > 0:
			for field in fields:
				self.fieldDict[field.name] = field

class PtrType(Type):
	def __init__(self, baseType, indLevel, mut):
		name = '{}{}{}'.format('&' * indLevel, 'mut ' if mut else '', baseType.name)
		super().__init__(name, PLATFORM_WORD_SIZE, PLATFORM_WORD_SIZE, 
			isIntLikeType=True, isPtrType=True, isPrimitiveType=True)
		self.baseType = baseType
		self.indLevel = indLevel
		self.mut = mut
		self.MIN = 0
		self.MAX = USZ_MAX
		self.RNG = USZ_RNG
	
	def typeAfterDeref(self, count=1):
		assert count > 0 and count <= self.indLevel
		if count == self.indLevel:
			return self.baseType
		else:
			return PtrType(self.baseType, self.indLevel - count, self.mut)

class ArrayFields:
	def __init__(self, elementType, count):
		self.count = count
		self.elementType = elementType
	
	def __getitem__(self, index):
		if index >= self.count:
			raise IndexError()
		return FieldInfo(str(index), self.elementType, index * getAlignedSize(self.elementType))
	
	def __len__(self):
		return self.count

class ArrayType(Type):
	def __init__(self, baseType, count):
		name = '[{} * {}]'.format(baseType.name, count)
		byteSize = getAlignedSize(baseType) * count
		super().__init__(name, byteSize, baseType.align, isArrayType=True, isCompositeType=True)
		self.baseType = baseType
		self.count = count
		self.fields = ArrayFields(baseType, count)

class TupleType(Type):
	def __init__(self, align, byteSize, fields):
		name = '({})'.format(', '.join(f.type.name for f in fields))
		super().__init__(name, byteSize, align, isTupleType=True, isCompositeType=True)
		self.fields = fields
		self.anon = True

SZ = PLATFORM_WORD_SIZE

UnknownType = Type('<unknown>', 0, 0, isResolved=False)

Void    = PrimitiveType('void',    0)
Bool    = PrimitiveType('bool',    1)
Byte    = PrimitiveType('byte',    1,   isIntLikeType=True)
Char    = PrimitiveType('char',    4,   isIntLikeType=True)
UInt8   = PrimitiveType('uint8',   1,   isIntType=True)
UInt16  = PrimitiveType('uint16',  2,   isIntType=True)
UInt32  = PrimitiveType('uint',    4,   isIntType=True)
UInt64  = PrimitiveType('uint64',  8,   isIntType=True)
USize   = PrimitiveType('usize',  SZ,   isIntType=True)
Int8    = PrimitiveType('int8',    1,   isIntType=True, isSigned=True)
Int16   = PrimitiveType('int16',   2,   isIntType=True, isSigned=True)
Int32   = PrimitiveType('int',     4,   isIntType=True, isSigned=True)
Int64   = PrimitiveType('int64',   8,   isIntType=True, isSigned=True)
ISize   = PrimitiveType('isize',  SZ,   isIntType=True, isSigned=True)
Float32 = PrimitiveType('float',   4, isFloatType=True, isSigned=True)
Float64 = PrimitiveType('float64', 8, isFloatType=True, isSigned=True)

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

Byte.MIN = 0
Byte.MAX = U8_MAX
Byte.RNG = U8_RNG
Char.MIN = 0
Char.MAX = U8_MAX
Char.RNG = U8_RNG
UInt8.MIN = 0
UInt8.MAX = U8_MAX
UInt8.RNG = U8_RNG
UInt16.MIN = 0
UInt16.MAX = U16_MAX
UInt16.RNG = U16_RNG
UInt32.MIN = 0
UInt32.MAX = U32_MAX
UInt32.RNG = U32_RNG
UInt64.MIN = 0
UInt64.MAX = U64_MAX
UInt64.RNG = U64_RNG
USize.MIN = 0
USize.MAX = USZ_MAX
USize.RNG = USZ_RNG
Int8.MIN = I8_MIN
Int8.MAX = I8_MAX
Int8.RNG = I8_RNG
Int16.MIN = I16_MIN
Int16.MAX = I16_MAX
Int16.RNG = I16_RNG
Int32.MIN = I32_MIN
Int32.MAX = I32_MAX
Int32.RNG = I32_RNG
Int64.MIN = I64_MIN
Int64.MAX = I64_MAX
Int64.RNG = I64_RNG
ISize.MIN = ISZ_MIN
ISize.MAX = ISZ_MAX
ISize.RNG = ISZ_RNG

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

def typesMatch(type1, type2):
	if type1 == type2:
		return True
	elif type1.isPtrType and type2.isPtrType and \
		typesMatch(type1.baseType, type2.baseType) and \
		type1.indLevel == type2.indLevel:
		return True
	elif type1.isOptionType and type2.isOptionType and \
		len(type1.types) == len(type2.types):
		for t in type1.types:
			if t not in type2.types:
				return False
		return True
	elif type1.isIntType and type2.isIntType and \
		type1.byteSize == type2.byteSize and type1.isSigned == type2.isSigned:
		return True
	elif type1.isCompositeType and type2.isCompositeType and shapesMatch(type1, type2):
		return True
	elif type1.isOwnedType and type2.isOwnedType and \
		type1.acquire == type2.acquire and type1.release == type2.release and \
		typesMatch(type1.baseType, type2.baseType):
		return True
	
	return False

def shapesMatch(type1, type2):
	if type1.byteSize != type2.byteSize or type1.align != type2.align or len(type1.fields) != len(type2.fields):
		return False
	
	for (f1, f2) in zip(type1.fields, type2.fields):
		if f1.offset != f2.offset or not typesMatch(f1.type, f2.type):
			return False
	
	return True

def canPromote(fromType, toType):
	if not (fromType and toType):
		return False
	elif fromType.isIntType and toType.isIntType and fromType.isSigned == toType.isSigned:
		return fromType.byteSize < toType.byteSize
	elif (fromType.isFloatType or fromType.isIntType) and toType.isFloatType:
		return fromType.byteSize < toType.byteSize

def canCoerce(fromType, toType):
	if fromType.isOwnedType or toType.isOwnedType:
		if not (fromType.isOwnedType and toType.isOwnedType):
			return False
		elif fromType.acquire != toType.acquire or fromType.release != toType.release:
			return False
		
		fromType = fromType.baseType
		toType = toType.baseType
	
	fromIsInt = fromType.isIntType or fromType in (Bool, Byte, Char)
	toIsInt = toType.isIntType or toType in (Bool, Byte, Char)
	if toType == Void:
		return True
	elif (fromType.isIntType or fromType.isFloatType) and \
		(toType.isIntType or toType.isFloatType):
		return True
	elif (fromType.isIntType or fromType.isPtrType) and \
		(toType.isIntType or toType.isPtrType):
		return True
	elif fromIsInt and toIsInt:
		return True
	else:
		return False

def hasDefiniteType(ast):
	if type(ast) == primitive.IntLit and (ast.suffix == None) and \
		(ast.value >= I32_MIN or ast.value <= I64_MAX):
		return False
	
	return True

def getAlignedSize(t):
	byteSize = t.byteSize
	if t.byteSize % t.align > 0:
		byteSize += t.byteSize - t.byteSize % t.align
	
	return byteSize
