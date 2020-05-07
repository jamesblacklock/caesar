from enum     import Enum
from .ast     import ValueSymbol, TypeModifiers
from .types   import FnType
from .ctlflow import Return
from .log     import logError
from .types   import getValidAssignType, Void
from .        import scope
from .        import attrs
from .block   import Block

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

class FnDecl(ValueSymbol):
	def __init__(self, nameTok, doccomment, extern, 
		params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(nameTok, None, span, doccomment, extern)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cVarArgsSpan = cVarArgsSpan
		self.returnTypeRef = returnType
		self.returnType = None
		self.returnTypeModifiers = TypeModifiers()
		self.body = body
		self.cconv = CConv.CAESAR
	
	def analyzeSig(decl, state):
		attrs.invokeAttrs(state, decl)
		
		if decl.returnTypeRef:
			decl.returnType = state.resolveTypeRef(decl.returnTypeRef)
		else:
			decl.returnType = Void
		
		symbolNames = set()
		for param in decl.params:
			param.type = state.resolveTypeRef(param.typeRef)
			if param.name in symbolNames:
				logError(state, param.span, 'duplicate parameter name')
			else:
				symbolNames.add(param.name)
		
		decl.type = FnType(decl.params, decl.returnType, decl.cVarArgs, decl.cconv)
		
		if decl.cVarArgs and decl.cconv != CConv.C:
			logError(state, decl.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')
		
		decl.mangledName = state.mangleName(decl)
	
	def analyze(decl, state, implicitType):
		if decl.extern:
			return
		
		decl.body.fnDecl = decl
		decl.body = state.analyzeNode(decl.body, decl.returnType)
		
		assert decl.body.doesReturn
	
	def pretty(self, output, indent=0):
		for attr in self.attrs:
			attr.pretty(output, indent)
			output.write('\n')
		if self.extern:
			output.write('extern fn ', indent)
		else:
			output.write('fn ', indent)
		
		output.write(self.name)
		output.write('(')
		if len(self.params) > 0:
			self.params[0].pretty(output)
			for param in self.params[1:]:
				output.write(', ')
				param.pretty(output)
		output.write(')')
		if self.returnTypeRef:
			output.write(' -> ')
			self.returnTypeRef.pretty(output)
		if self.body:
			self.body.pretty(output, indent)
