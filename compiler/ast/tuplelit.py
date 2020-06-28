from .ast               import AST
from ..types            import Void, TupleType, ArrayType, getAlignedSize
from ..mir.createstruct import CreateStruct, FieldInit

class TupleLit(AST):
	def __init__(self, values, span):
		super().__init__(span, True)
		self.values = values
	
	def analyze(self, state, implicitType):
		if implicitType and implicitType.isCompositeType:
			expectedTypes = [f.type for f in implicitType.fields]
		else:
			expectedTypes = [None for _ in self.values]
		
		fieldFailed = False
		resolvedTypes = []
		accesses = []
		for (expr, t) in zip(self.values, expectedTypes):
			access = state.analyzeNode(expr, t)
			if access and access.type:
				accesses.append(access)
				resolvedTypes.append(access.type)
			else:
				fieldFailed = True
		
		if fieldFailed:
			return None
		
		layout = state.generateFieldLayout(resolvedTypes)
		type = TupleType(layout.align, layout.byteSize, layout.fields)
		inits = [FieldInit(access, field.offset) for (access, field) in zip(accesses, layout.fields)]
		return CreateStruct(inits, type, self.span)

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
		inits = [FieldInit(access, alignedSize * i) for (i, access) in enumerate(accesses)]
		return CreateStruct(inits, type, self.span)
