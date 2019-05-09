class Type:
	def __init__(self, name, byteSize):
		self.name = name
		self.byteSize = byteSize

PLATFORM_WORD_SIZE = 8

Void    = Type('Void',    0)
Bool    = Type('Bool',    1)
Byte    = Type('Byte',    1)
Int8    = Type('Int8',    1)
UInt8   = Type('UInt8',   1)
Int16   = Type('Int16',   2)
UInt16  = Type('UInt16',  2)
Int32   = Type('Int32',   4)
UInt32  = Type('UInt32',  4)
Char    = Type('Char',    4)
Int64   = Type('Int64',   8)
UInt64  = Type('UInt64',  8)
ISize   = Type('ISize',   PLATFORM_WORD_SIZE)
USize   = Type('USize',   PLATFORM_WORD_SIZE)
Float32 = Type('Float32', 4)
Float64 = Type('Float64', 8)
Ptr     = Type('$Ptr',    PLATFORM_WORD_SIZE)

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
