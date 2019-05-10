class ResolvedType:
	def __init__(self, name, byteSize, isFnType=False, isMultiIntType=False, isPtrType=False):
		self.name = name
		self.byteSize = byteSize
		self.isFnType = isFnType
		self.isMultiIntType = isMultiIntType
		self.isPtrType = isPtrType
	
	def __str__(self):
		return self.name

class ResolvedFnType(ResolvedType):
	def __init__(self, resolvedParamTypes, cVarArgs, resolvedReturnType):
		name = 'fn ({}) -> {}'.format(
			', '.join([t.name for t in resolvedParamTypes]), 
			resolvedReturnType.name)
		super().__init__(name, 0, isFnType=True)
		self.resolvedReturnType = resolvedReturnType
		self.resolvedParamTypes = resolvedParamTypes
		self.cVarArgs = cVarArgs

PLATFORM_WORD_SIZE = 8

class ResolvedPtrType(ResolvedType):
	def __init__(self, baseType, indirectionLevel):
		name = baseType.name + ('^' * indirectionLevel)
		super().__init__(name, PLATFORM_WORD_SIZE, isPtrType=True)
		self.baseType = baseType
		self.indirectionLevel = indirectionLevel

Void    = ResolvedType('Void',    0)
Bool    = ResolvedType('Bool',    1)
Byte    = ResolvedType('Byte',    1)
Int8    = ResolvedType('Int8',    1)
UInt8   = ResolvedType('UInt8',   1)
Int16   = ResolvedType('Int16',   2)
UInt16  = ResolvedType('UInt16',  2)
Int32   = ResolvedType('Int32',   4)
UInt32  = ResolvedType('UInt32',  4)
Char    = ResolvedType('Char',    4)
Int64   = ResolvedType('Int64',   8)
UInt64  = ResolvedType('UInt64',  8)
ISize   = ResolvedType('ISize',   PLATFORM_WORD_SIZE)
USize   = ResolvedType('USize',   PLATFORM_WORD_SIZE)
Float32 = ResolvedType('Float32', 4)
Float64 = ResolvedType('Float64', 8)

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

class ResolvedMultiIntType(ResolvedType):
	def __init__(self, value):
		super().__init__('integer', 0, isMultiIntType=True)
		possibleTypes = []
		if value <= I8_MAX and value >= I8_MIN:
			possibleTypes.append(Int8)
		if value <= U8_MAX and value >= 0:
			possibleTypes.append(UInt8)
		if value <= I16_MAX and value >= I16_MIN:
			possibleTypes.append(Int16)
		if value <= U16_MAX and value >= 0:
			possibleTypes.append(UInt16)
		if value <= I32_MAX and value >= I32_MIN:
			possibleTypes.append(Int32)
		if value <= U32_MAX and value >= 0:
			possibleTypes.append(UInt32)
		if value <= I64_MAX and value >= I64_MIN:
			possibleTypes.append(Int64)
		if value <= U64_MAX and value >= 0:
			possibleTypes.append(UInt64)
		if value <= ISZ_MAX and value >= ISZ_MIN:
			possibleTypes.append(ISize)
		if value <= USZ_MAX and value >= 0:
			possibleTypes.append(USize)
		
		self.possibleTypes = possibleTypes
	
	def __str__(self):
		return self.possibleTypes[0].__str__() if len(self.possibleTypes) > 0 else self.name

def typesMatch(expectedType, foundType):
	if expectedType == None:
		return True
	
	if type(expectedType) == ResolvedMultiIntType:
		expectedType, foundType = foundType, expectedType
	
	if type(foundType) == ResolvedMultiIntType:
		if type(expectedType) == ResolvedMultiIntType:
			for t in foundType.possibleTypes:
				if t in expectedType.possibleTypes:
					return True
			
			return False
		else:
			return expectedType in foundType.possibleTypes
	elif type(expectedType) == ResolvedPtrType and type(foundType) == ResolvedPtrType:
		return expectedType.baseType == foundType.baseType and \
			expectedType.indirectionLevel == foundType.indirectionLevel
	else:
		return expectedType is foundType
