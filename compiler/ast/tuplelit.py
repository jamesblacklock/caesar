from .ast               import AST
from ..types            import Void, ArrayType, getAlignedSize
from ..not_done.tupledecl import TupleDecl
from ..mir.createstruct import CreateStruct, FieldInit

class TupleLit(AST):
	def __init__(self, values, span):
		super().__init__(span, True)
		self.values = values
	
	def analyze(self, state, implicitType):
		# uninitFields = set()
		resolvedType = implicitType
		
		if resolvedType:
			if resolvedType.isTupleType:
				# fieldDict = resolvedType.fieldDict
				uninitFields = { f for f in resolvedType.fields }
			else:
				logError(state, self.span, 'type `{}` is not a tuple type'.format(resolvedType.name))
				resolvedType = None
		
		expectedTypes = []
		if resolvedType:
			expectedTypes = [field.type for field in resolvedType.fields]
		
		while len(expectedTypes) < len(self.values):
			expectedTypes.append(None)
		
		fieldFailed = False
		# initFields = {}
		accesses = []
		# for (value, fieldType, i) in zip(self.values, expectedTypes, range(0, len(expectedTypes))):
		for (value, fieldType) in zip(self.values, expectedTypes):
			# if fieldType:
				# fieldSymbol = resolvedType.fields[i]
				# initFields[fieldSymbol] = fieldInit
				# uninitFields.remove(fieldSymbol)
			
			access = state.analyzeNode(value, fieldType)
			if access:
				access = state.typeCheck(access, fieldType)
				accesses.append(access)
			else:
				fieldFailed = True
		
		if fieldFailed:
			return None
		
		if resolvedType == None:
			layout = state.generateFieldLayout([access.type for access in accesses])
			resolvedType = TupleDecl.generateAnonTupleDecl(layout)
		
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
		return CreateStruct(inits, type, self.span)
