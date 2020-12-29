from .ast               import AST
from ..types            import USize, Void, ArrayType, getAlignedSize, PtrType
from ..symbol.tuple     import TupleType
from ..mir.createstruct import CreateStruct, FieldInit
from ..mir.primitive    import IntValue
from ..mir.access       import SymbolAccess
from ..log              import logError
from ..span             import Span

class TupleLit(AST):
	def __init__(self, values, span, resolvedType=None):
		super().__init__(span, True)
		self.values = values
		self.resolvedType = resolvedType
	
	def analyze(self, state, implicitType):
		if self.resolvedType:
			implicitType = self.resolvedType
		
		expectedTypes = []
		if implicitType:
			if implicitType.isTupleType:
				expectedTypes = [field.type for field in implicitType.fields]
			else:
				implicitType = None
		
		while len(expectedTypes) < len(self.values):
			expectedTypes.append(None)
		
		fieldFailed = False
		tooMany = None
		accesses = []
		for (value, fieldType) in zip(self.values, expectedTypes):
			access = state.analyzeNode(value, fieldType)
			if access:
				if implicitType:
					if fieldType == None:
						tooMany = Span.merge(tooMany, value.span) if tooMany else value.span
					else:
						access = state.typeCheck(access, fieldType)
				accesses.append(access)
			else:
				fieldFailed = True
		
		if tooMany:
			logError(state, tooMany, 'too many fields found for type `{}` (expected {}, found {})'.format(
				implicitType.name, len(implicitType.fields), len(self.values)))
		
		if fieldFailed:
			return None
		
		resolvedType = implicitType
		if resolvedType == None:
			layout = state.generateFieldLayout([access.type for access in accesses])
			resolvedType = TupleType.generateAnonTupleType(layout)
		
		inits = []
		for (access, field) in zip(accesses, resolvedType.fields):
			inits.append(FieldInit(access, field))
		
		return CreateStruct(inits, resolvedType, self.span)

class ArrayLit(AST):
	def __init__(self, values, span):
		super().__init__(span, True)
		self.values = values
	
	def analyze(self, state, implicitType):
		implicitElementType = Void
		count = len(self.values)
		if implicitType and implicitType.isArrayType:
			implicitElementType = implicitType.baseType
			count = max(count, implicitType.count)
		
		fieldFailed = False
		accesses = []
		noTypeDetected = True
		for expr in self.values:
			access = state.analyzeNode(expr, implicitElementType)
			if access:
				if noTypeDetected and access.type:
					noTypeDetected = False
					elementType = access.type
					implicitElementType = elementType
				access = state.typeCheck(access, implicitElementType)
				accesses.append(access)
			else:
				fieldFailed = True
		
		if fieldFailed:
			return None
		
		elementType = implicitElementType
		type = ArrayType(elementType, count)
		alignedSize = getAlignedSize(elementType)
		inits = [FieldInit(access, type.fields[i]) for (i, access) in enumerate(accesses)]
		dataStruct = CreateStruct(inits, type, self.span)
		
		assert implicitType and implicitType.name == 'arr'
		
		ArrType = implicitType
		arrDataPtr = SymbolAccess.read(state, dataStruct)
		arrDataPtr.symbol.mut = True
		arrDataPtr.ref = False
		arrDataPtr.addr = True
		arrDataPtr.type = PtrType(arrDataPtr.type, 1, True)
		
		structValue = CreateStruct.create(state, ArrType, self.span, [
			CreateStruct.init('count', IntValue(count, USize, self.span)),
			CreateStruct.init('data', arrDataPtr)
		])
		
		return structValue
