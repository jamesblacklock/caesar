from .ast            import AST, Symbol
from ..symbol.struct import Struct

class UnionFields(AST):
	def __init__(self, fields, span):
		super().__init__(span)
		self.fieldDecls = fields

class FieldDecl(AST):
	def __init__(self, name, typeRef, pub, mut, span):
		super().__init__(span)
		self.nameSpan = name.span
		self.name = name.content
		self.typeRef = typeRef
		self.align = None
		self.offset = None
		self.unionField = False
		self.noOffset = False
		self.pub = pub
		self.mut = mut

class StructDecl(Symbol):
	def __init__(self, name, isUnion, doccomment, fields, pub, span):
		super().__init__(name, span, doccomment)
		self.fieldDecls = fields
		self.isUnion = isUnion
		self.pub = pub
		self.anon = not self.name
	
	def createSymbol(self, state):
		return Struct(self)
	
	def flattenedFieldDecls(self):
		flattened = []
		for (i, fieldDecl) in enumerate(self.fieldDecls):
			if type(fieldDecl) == UnionFields:
				for (j, unionField) in enumerate(fieldDecl.fieldDecls):
					unionField.unionField = True
					if j+1 < len(fieldDecl.fieldDecls):
						unionField.noOffset = True
					flattened.append(unionField)
			else:
				if self.isUnion:
					fieldDecl.unionField = True
					if i+1 < len(self.fieldDecls):
						fieldDecl.noOffset = True
				flattened.append(fieldDecl)
		
		return flattened