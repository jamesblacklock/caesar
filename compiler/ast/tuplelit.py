from .ast               import ValueExpr
from ..types            import Void, TupleType, ArrayType, getAlignedSize
from ..mir.createstruct import CreateStruct, FieldInit

class TupleLit(ValueExpr):
	def __init__(self, values, span):
		super().__init__(span)
		self.values = values
	
	def analyze(self, state, implicitType):
		# requireTypeMatch = False
		# if self.type:
		# 	expectedTypes = [f.type for f in self.type.fields]
		# 	requireTypeMatch = True
		# el
		if implicitType and implicitType.isCompositeType:
			expectedTypes = [f.type for f in implicitType.fields]
		else:
			expectedTypes = [None for _ in self.values]
		
		# if requireTypeMatch and len(self.values) != len(expectedTypes):
		# 	logError(state, self.span, 'expected {} initializers for type `{}`, found {}'.format(
		# 		len(expectedTypes), self.type.name, len(self.values)))
		# 	while len(expectedTypes) < len(self.values):
		# 		expectedTypes.append(None)
		
		resolvedTypes = []
		accesses = []
		for (expr, t) in zip(self.values, expectedTypes):
			access = state.analyzeNode(expr, t)
			# if requireTypeMatch:
			# 	expr = state.typeCheck(expr, t)
			accesses.append(access)
			resolvedTypes.append(access.type)
		
		# if not self.type:
		layout = state.generateFieldLayout(resolvedTypes)
		type = TupleType(layout.align, layout.byteSize, layout.fields)
		inits = [FieldInit(access, field.offset) for (access, field) in zip(accesses, layout.fields)]
		return CreateStruct(inits, type, self.span)

class ArrayLit(ValueExpr):
	def __init__(self, values, span):
		super().__init__(span)
		self.values = values
	
	def analyze(self, state, implicitType):
		implicitElementType = Void
		count = len(self.values)
		if implicitType and implicitType.isArrayType:
			implicitElementType = implicitType.baseType
			count = max(count, implicitType.count)
		
		accesses = []
		noTypeDetected = True
		for expr in self.values:
			access = state.analyzeNode(expr, implicitElementType)
			if noTypeDetected and access.type:
				noTypeDetected = False
				elementType = access.type
				implicitElementType = elementType
			access = state.typeCheck(access, implicitElementType)
			accesses.append(access)
		
		elementType = implicitElementType
		type = ArrayType(elementType, count)
		alignedSize = getAlignedSize(elementType)
		inits = [FieldInit(access, alignedSize * i) for (i, access) in enumerate(accesses)]
		return CreateStruct(inits, type, self.span)
