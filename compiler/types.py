from .mir.mir    import TypeModifiers, GENERIC_STATIC_DATA
from .mir.coerce import Coerce

PLATFORM_WORD_SIZE = 8

class Type:
	def __init__(self, name=None, span=None, symbol=None,
		byteSize=None, align=None, isDefinite=True, isEnumType=False, 
		isFnType=False, isPtrType=False, isStructType=False, isTraitType=False, 
		isIntType=False, isIntLikeType=False, isFloatType=False, isOptionType=False, 
		isPrimitiveType=False, isSigned=False, isArrayType=False, isOwnedType=False, 
		isTupleType=False, isCompositeType=False, isGenericType=False, isUnknown=False):#, isTypeDef=False):
		self.name = name
		self.span = span
		self.symbol = symbol
		self.byteSize = byteSize
		self.align = align
		self.isDefinite = isDefinite
		self.isUnknown = isUnknown
		self.isPrimitiveType = isPrimitiveType
		self.isCopyable = isFnType or isPrimitiveType or isPtrType
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
		self.isEnumType = isEnumType
		self.isOwnedType = isOwnedType
		self.isTraitType = isTraitType
		# self.isTypeDef = isTypeDef
		self.dropFn = None
		self.symbolTable = {}
		self.traitImpls = {}
		self.isGenericType = isGenericType
	
	@property
	def isVoidType(self):
		return self.byteSize == 0
	
	@property
	def isUnsized(self):
		return self.byteSize == None
	
	def updateName(self):
		pass
	
	def canChangeTo(self, other):
		if not self.isDefinite and self.isCompositeType and other.isCompositeType:
			return shapesMatch(self, other)
		
		return False
	
	def resolveGenerics(self, genericInc):
		if self.isGenericType:
			assert 0
		return self
	
	def refGenericType(self, state):
		if self.isGenericType:
			assert 0
	
	def __str__(self):
		if self.name == None:
			self.updateName()
		return self.name

class OwnedType(Type):
	def __init__(self, baseType, acquire, release, acquireSpan, releaseSpan, span):
		super().__init__(
			span=span, 
			byteSize=baseType.byteSize, 
			align=baseType.align, 
			isOwnedType=True,
			isGenericType=baseType.isGenericType)
		self.baseType = baseType
		self.acquire = acquire
		self.release = release
		self.acquireSpan = acquireSpan
		self.releaseSpan = releaseSpan
	
	def typeAfterDeref(self, count=1):
		assert self.baseType.isPtrType
		return self.baseType.typeAfterDeref(count)
	
	def updateName(self):
		self.baseType.updateName()
		self.name = 'owned({}, {}) {}'.format(
			self.acquire.name if self.acquire else '<unknown>', 
			self.release.name if self.release else '<unknown>', self.baseType.name)
	
	def resolveGenerics(self, genericInc):
		if not self.isGenericType:
			return self
		return OwnedType(
			self.baseType.resolveGenerics(genericInc), 
			self.acquire, 
			self.release, 
			self.acquireSpan, 
			self.releaseSpan, 
			self.span)

	def refGenericType(self, state):
		if self.isGenericType:
			self.baseType.refGenericType(state)

class PrimitiveType(Type):
	def __init__(self, name, byteSize, isIntType=False, 
		isIntLikeType=False, isFloatType=False, isSigned=False):
		super().__init__(
			name=name, 
			byteSize=byteSize, 
			align=byteSize, 
			isPrimitiveType=True, 
			isIntLikeType=isIntLikeType, 
			isIntType=isIntType, 
			isFloatType=isFloatType, 
			isSigned=isSigned)
		self.mangledName = 'T{}{}'.format(len(name), name)

class OptionType(Type):
	def __init__(self, *types):
		align = max(t.align for t in types)
		name = '|'.join(t.name for t in types)
		byteSize = max(t.byteSize for t in types)
		super().__init__(
			name=name, 
			byteSize=byteSize, 
			align=align, 
			isOptionType=True)
		self.types = types
		self.isGenericType = len([None for t in types if t.isGenericType]) > 0
	
	def resolveGenerics(self, genericInc):
		if not self.isGenericType:
			return self
		return OptionType([t.resolveGenerics(genericInc) for t in self.types])

class FnType(Type):
	def __init__(self, unsafe, params, returnType, cVarArgs, cconv):
		super().__init__(
			name='fn(???) -> ???',
			byteSize=PLATFORM_WORD_SIZE,
			align=PLATFORM_WORD_SIZE,
			isFnType=True)
		self.unsafe = unsafe
		self.returnType = returnType
		self.returnTypeModifiers = TypeModifiers(False)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cconv = cconv
		self.isGenericType = returnType and returnType.isGenericType
		for p in params:
			if self.isGenericType:
				break
			self.isGenericType = p.type and p.type.isGenericType
	
	def updateName(self):
		for p in self.params: p.type.updateName()
		self.returnType.updateName()
		self.name = '{}fn({}{}{}) -> {}'.format(
			'unsafe ' if self.unsafe else '',
			', '.join([t.type.name for t in self.params]), 
			', ' if self.cVarArgs and len(self.params) > 0 else '',
			'...' if self.cVarArgs else '',
			self.returnType.name)
	
	def resolveGenerics(self, genericInc):
		if not self.isGenericType:
			return self
		return FnType(
			self.unsafe, 
			[p.resolveGenericParam(genericInc) for p in self.params], 
			self.returnType.resolveGenerics(genericInc), 
			self.cVarArgs, 
			self.cconv)

class FieldInfo:
	def __init__(self, name, symbolType, offset, isUnionField=False, noOffset=False, pub=True, mut=True):
		self.name = name
		self.type = symbolType
		self.offset = offset
		self.isUnionField = isUnionField
		self.noOffset = noOffset
		self.pub = pub
		self.mut = mut

class PtrType(Type):
	def __init__(self, baseType, indLevel, mut):
		isTraitPtr = baseType.isTraitType and indLevel == 1
		byteSize=PLATFORM_WORD_SIZE * (2 if isTraitPtr else 1)
		align=PLATFORM_WORD_SIZE * (2 if isTraitPtr else 1)
		
		super().__init__(
			byteSize=byteSize, 
			align=align, 
			isIntLikeType=not isTraitPtr, 
			isPrimitiveType=not isTraitPtr, 
			isPtrType=True, 
			isCompositeType=isTraitPtr,
			isGenericType=baseType.isGenericType)
		
		if isTraitPtr:
			self.fields = [
				FieldInfo('$self', PtrType(Void, 1, mut), PLATFORM_WORD_SIZE), 
				FieldInfo('$vtbl', PtrType(Void, 1, False), PLATFORM_WORD_SIZE)
			]
			self.fieldDict = {
				self.fields[0].name: self.fields[0], 
				self.fields[1].name: self.fields[1]
			}
		self.isTraitPtr = isTraitPtr
		self.baseType = baseType
		self.indLevel = indLevel
		self.mut = mut
		self.MIN = 0
		self.MAX = USZ_MAX
		self.RNG = USZ_RNG
	
	def updateName(self):
		self.baseType.updateName()
		self.name = '{}{}{}'.format('&' * self.indLevel, 'mut ' if self.mut else '', self.baseType.name)
	
	def typeAfterDeref(self, count=1):
		assert count > 0 and count <= self.indLevel
		if count == self.indLevel:
			return self.baseType
		else:
			return PtrType(self.baseType, self.indLevel - count, self.mut)
	
	def resolveGenerics(self, genericInc):
		if not self.isGenericType:
			return self
		return PtrType(self.baseType.resolveGenerics(genericInc), self.indLevel, self.mut)
	
	def refGenericType(self, state):
		if self.isGenericType:
			self.baseType.refGenericType(state)

class ArrayFields:
	def __init__(self, elementType, count):
		self.count = count
		self.elementType = elementType
	
	def __contains__(self, index):
		try:
			return int(index) < self.count
		except:
			return False
	
	def __getitem__(self, index):
		index = int(index)
		if index >= self.count:
			raise IndexError()
		return FieldInfo(str(index), self.elementType, index * getAlignedSize(self.elementType))
	
	def __len__(self):
		return self.count

class ArrayType(Type):
	def __init__(self, baseType, count):
		isGenericType = baseType.isGenericType or count == GENERIC_STATIC_DATA
		
		if baseType.isGenericType and baseType.byteSize == None or count == GENERIC_STATIC_DATA:
			byteSize = None
			count = None
		else:
			byteSize = getAlignedSize(baseType) * count
		super().__init__(
			byteSize=byteSize, 
			align=baseType.align, 
			isArrayType=True, 
			isCompositeType=True,
			isGenericType=isGenericType)
		self.baseType = baseType
		self.count = count
		self.fields = ArrayFields(baseType, count)
		self.fieldDict = self.fields
		self.anon = True
	
	def updateName(self):
		self.baseType.updateName()
		self.name = '[{} * {}]'.format(self.baseType.name, self.count)
	
	def resolveGenerics(self, genericInc):
		if not self.isGenericType:
			return self
		return ArrayType(self.baseType.resolveGenerics(genericInc), self.count)
	
	def refGenericType(self, state):
		if self.isGenericType:
			self.baseType.refGenericType(state)

# class IndefiniteIntType(Type):
# 	def __init__(self, initialValue):
		
# 		elif access.symbol.type.isIntLikeType and implicitType.isIntLikeType:
# 			doChangeType = access.symbol.type.canChangeTo(implicitType)
		
# 		if access.symbol.type.canChangeTo(implicitType):

SZ = PLATFORM_WORD_SIZE

# UnknownType = Type(
# 	name='<unknown>', 
# 	byteSize=0, 
# 	align=0, 
# 	isUnknown=True)

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

BUILTIN_TYPES = {
	Void.name:    Void,
	Bool.name:    Bool,
	Byte.name:    Byte,
	Int8.name:    Int8,
	UInt8.name:   UInt8,
	Int16.name:   Int16,
	UInt16.name:  UInt16,
	Int32.name:   Int32,
	UInt32.name:  UInt32,
	Char.name:    Char,
	Int64.name:   Int64,
	UInt64.name:  UInt64,
	ISize.name:   ISize,
	USize.name:   USize,
	Float32.name: Float32,
	Float64.name: Float64
}

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

class FieldLayout:
	def __init__(self, align, byteSize, fields):
		self.align = align
		self.byteSize = byteSize
		self.fields = fields

def generateFieldLayout(types, fieldNames=None, fieldInfo=None, genericInc=None):
	fields = []
	maxAlign = 0
	offset = 0
	
	if fieldNames == None:
		fieldNames = (str(i) for i in range(0, len(types)))
	if fieldInfo == None:
		fieldInfo = (None for _ in types)
	
	unionSize = 0
	sizeUnknown = False
	
	for (t, n, f) in zip(types, fieldNames, fieldInfo):
		if genericInc:
			t = t.resolveGenerics(genericInc)
			assert t.byteSize != None
		
		sizeUnknown = sizeUnknown or t.byteSize == None
		
		if t.isVoidType:
			continue
		
		byteSize = t.byteSize if t.byteSize != None else 0
		align = t.align if t.align != None else 1
		
		maxAlign = max(maxAlign, align)
		
		if offset % align > 0:
			offset += align - offset % align
		
		isUnionField = f.unionField if f else False
		noOffset     =   f.noOffset if f else False
		pub          =        f.pub if f else True
		mut          =        f.mut if f else True
		
		fields.append(FieldInfo(n, t, offset, isUnionField, noOffset, pub, mut))
		
		if noOffset:
			unionSize = max(unionSize, byteSize)
		else:
			offset += max(unionSize, byteSize)
			unionSize = 0
	
	if unionSize > 0:
		offset += unionSize
	
	byteSize = None if sizeUnknown else offset
	return FieldLayout(maxAlign, byteSize, fields)

def allFields(t):
	if not t.isCompositeType:
		return set()
	
	fields = set(t.fields)
	for f in t.fields:
		fields.update(allFields(f.type))
	
	return fields

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

def typesMatch(type1, type2, selfType=None):
	assert type1 and type2 and not (type1.isUnknown or type2.isUnknown)
	
	if type1 == type2:
		return True
	elif not type1.isGenericType and type2.isGenericType and type1.symbol and type1.symbol.paramType == type2.symbol.paramType:
		# This is for a (hacky?) special case where the concrete type is irrelevant to code generation
		# due to being accessed solely via a pointer.
		# TODO: make sure that this can't possible slip through in any other situation!
		return True
	elif type1.isPtrType and type2.isPtrType:
		if type1.indLevel != type2.indLevel:
			return False
		return type1.mut == type2.mut and typesMatch(type1.baseType, type2.baseType)
	elif type1.isOptionType and type2.isOptionType:
		if len(type1.types) != len(type2.types):
			return False
		for t in type1.types:
			if t not in type2.types:
				return False
		return True
	elif type1.isIntType and type2.isIntType:
		return type1.byteSize == type2.byteSize and type1.isSigned == type2.isSigned
	elif type1.isOwnedType and type2.isOwnedType:
		if type1.acquire != type2.acquire or type1.release != type2.release:
			return False
		return typesMatch(type1.baseType, type2.baseType)
	elif type1.isCompositeType and type2.isCompositeType:
		return type1.anon and type2.anon and shapesMatch(type1, type2)
	elif type1.isFnType and type2.isFnType:
		if type1.unsafe != type2.unsafe or type1.cVarArgs != type2.cVarArgs or type1.cconv != type2.cconv:
			return False
		if not typesMatch(type1.returnType, type2.returnType):
			return False
		if len(type1.params) != len(type2.params):
			return False
		isSelfParam = selfType != None
		for (p1, p2) in zip(type1.params, type2.params):
			t1 = p1.type
			t2 = p2.type
			if isSelfParam:
				isSelfParam = False
				if typesMatch(t1, selfType):
					continue
				elif not (t2.isPtrType and t1.isPtrType and t1.indLevel == 1 and t2.indLevel == 1):
					pass
				elif not (t2.isTraitPtr and t2.baseType in t1.baseType.traitImpls):
					pass
				elif typesMatch(t1.baseType, selfType):
					continue
			
			if not typesMatch(t1, t2):
				return False
		return True
	
	return False

def shapesMatch(type1, type2):
	if not (type1 and type2 and type1.isCompositeType and type2.isCompositeType):
		return False
	
	if type1.byteSize != type2.byteSize or type1.align != type2.align or len(type1.fields) != len(type2.fields):
		return False
	
	for (f1, f2) in zip(type1.fields, type2.fields):
		if f1.offset != f2.offset or f1.name != f2.name or not typesMatch(f1.type, f2.type):
			return False
	
	return True

def tryPromote(state, access, toType):
	fromType = access.type
	if not fromType or not toType or fromType == toType:
		return access
	
	# int/float promotions
	intToInt = fromType.isIntType and toType.isIntType
	intOrFloatToFloat = (fromType.isIntType or fromType.isFloatType) and toType.isFloatType
	if intToInt or intOrFloatToFloat:
		signsMatch = fromType.isSigned == toType.isSigned
		sizeDoesIncrease = fromType.byteSize < toType.byteSize
		if signsMatch and sizeDoesIncrease:
			access = state.analyzeNode(Coerce(access, toType, access.span))
			access.dropPoint = state.dropPoint
			return access
	
	# pointer promotions
	fromBaseType = fromType
	toBaseType = toType
	if fromBaseType.isOwnedType and toBaseType.isOwnedType and \
		fromBaseType.release == toBaseType.release and \
		fromBaseType.acquire == toBaseType.acquire:
		fromBaseType = fromBaseType.baseType
		toBaseType = toBaseType.baseType
	
	if fromBaseType.isPtrType and toBaseType.isPtrType:
		toDerefType = toBaseType.typeAfterDeref()
		fromDerefType = fromBaseType.typeAfterDeref()
		if toDerefType == Void and (fromBaseType.mut or not toBaseType.mut):
			access.type = toType
			return access
		elif typesMatch(fromDerefType, toDerefType) and not toBaseType.mut:
			access.type = toType
			return access
		elif toDerefType.isTraitType and toDerefType in fromDerefType.traitImpls:
			access = state.analyzeNode(Coerce(access, toType, access.span))
			access.dropPoint = state.dropPoint
			return access
	
	# promote scalar to tuple
	if toType.isTupleType and len(toType.fields) == 1 and typesMatch(toType.fields[0].type, fromType):
		access.type = toType
		return access
	
	# if type(access) == SymbolAccess and access.ref and indefiniteMatch(access.type, expectedType):
	# 	access.symbol.type = expectedType
	# 	access.type = expectedType
	# 	return access
	
	# if toType.isOptionType:
	# 	for t in toType.types:
	# 		if typesMatch(fromType, t):
	# 			return constructOptionInstance(access)
	
	return access

def canCoerce(fromType, toType):
	if fromType.isOwnedType or toType.isOwnedType:
		if not (fromType.isOwnedType and toType.isOwnedType):
			return False
		elif fromType.acquire != toType.acquire or fromType.release != toType.release:
			return False
		
		fromType = fromType.baseType
		toType = toType.baseType
	
	# while fromType.isTypeDef:
	# 	fromType = fromType.baseType
	# while toType.isTypeDef:
	# 	toType = toType.baseType
	
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
	elif fromType.isCompositeType and toType.isCompositeType:
		return shapesMatch(fromType, toType)
	elif toType.isTupleType and len(toType.fields) == 1 and typesMatch(toType.fields[0].type, fromType):
		return True
	else:
		return False

def hasDefiniteType(ast):
	from .ast import primitive
	if type(ast) == primitive.IntLit and (ast.suffix == None) and \
		(ast.value >= I32_MIN or ast.value <= I64_MAX):
		return False
	
	return True

def getAlignedSize(t):
	byteSize = t.byteSize
	if t.byteSize % t.align > 0:
		byteSize += t.align - t.byteSize % t.align
	
	return byteSize
