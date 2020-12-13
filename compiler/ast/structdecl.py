from .ast               import AST, SymbolAST
from ..symbol.struct    import Struct
from ..symbol.paramtype import ParamTypeSymbol

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

class StructDecl(SymbolAST):
	def __init__(self, name, isUnion, doccomment, typeParams, fields, pub, span):
		super().__init__(name, span, doccomment)
		self.typeParams = typeParams
		self.fieldDecls = fields
		self.isUnion = isUnion
		self.pub = pub
		self.anon = not self.name
	
	def createSymbol(self, state):
		if self.typeParams:
			return ParamTypeSymbol(self, self.typeParams) 
		return Struct(self)
	
	def resolveSig(self, state):
		symbol = self.createSymbol(state)
		return symbol.type
	
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