class ResolvedType:
	def __init__(self, name, byteSize, isFnType=False, isMultiIntType=False):
		self.name = name
		self.byteSize = byteSize
		self.isFnType = isFnType
		self.isMultiIntType = isMultiIntType
	
	def __str__(self):
		return self.name

class ResolvedFnType(ResolvedType):
	def __init__(self, resolvedParamTypes, cVarArgs, resolvedReturnType):
		super().__init__('function', 0, isFnType=True)
		self.resolvedReturnType = resolvedReturnType
		self.resolvedParamTypes = resolvedParamTypes
		self.cVarArgs = cVarArgs

PLATFORM_WORD_SIZE = 8

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
Ptr     = ResolvedType('$Ptr',    PLATFORM_WORD_SIZE)

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
	Float64,
	Ptr
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

def typesMatch(t1, t2):
	if type(t2) == ResolvedMultiIntType:
		t1, t2 = t2, t1
	
	if type(t1) == ResolvedMultiIntType:
		if type(t2) == ResolvedMultiIntType:
			for t in t1.possibleTypes:
				if t in t2.possibleTypes:
					return True
			
			return False
		else:
			return t2 in t1.possibleTypes
	
	return t1 and t2 and (t1 is t2)
