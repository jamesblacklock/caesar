from .ast       import AST
from ..types    import USize
from .primitive import IntValue
from ..log      import logError

class Sizeof(AST):
	def __init__(self, typeRef, span):
		super().__init__(span)
		self.typeRef = typeRef
	
	def analyze(self, state, implicitType):
		type = state.resolveTypeRef(self.typeRef)
		if type == None:
			return None
		elif type.byteSize == None:
			logError(state, self.typeRef.span, 'type `{}` is unsized'.format(type.name))
			return None
		else:
			return IntValue(type.byteSize, USize, self.span)

class Offsetof(AST):
	def __init__(self, typeRef, path, span):
		super().__init__(span)
		self.typeRef = typeRef
		self.path = path
	
	def analyze(self, state, implicitType):
		type = state.resolveTypeRef(self.typeRef)
		if type == None:
			return None
		elif not type.isCompositeType:
			logError(state, self.typeRef.span, 'type `{}` has no fields'.format(type.name))
			return None
		else:
			t = type
			field = None
			offset = 0
			for name in self.path:
				if not t.isCompositeType:
					logError(state, self.typeRef.span, 'type `{}` has no fields'.format(t.name))
					return None
				elif name.content not in t.fieldDict:
					logError(state, name.span, 'type `{}` has no field `{}`'.format(t.name, name.content))
					return None
				
				field = t.fieldDict[name.content]
				offset += field.offset
				t = field.type
			
			return IntValue(offset, USize, self.span)
