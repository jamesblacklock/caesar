from .ast         import ValueSymbolAST
from ..symbol.fn  import Fn, CConv
from ..log        import logError, logExplain
from .genericinst import GenericAssocConst, GenericAssocType, GenericType

class FnDecl(ValueSymbolAST):
	def __init__(self, name, doccomment, pub, extern, cconv, unsafe, 
		params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(name, None, span, doccomment, extern)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cVarArgsSpan = cVarArgsSpan
		self.returnTypeRef = returnType
		self.body = body
		self.cconv = cconv
		self.unsafe = unsafe
		self.pub = pub
		self.alwaysInline = False
		self.neverInline = False
	
	def createSymbol(self, state):
		symbolNames = {}
		params = []
		for param in self.params:
			if param.name in symbolNames:
				logError(state, param.span, 'duplicate parameter name')
				logExplain(state, symbolNames[param.name], '`{}` was previously declared here'.format(param.name))
			else:
				symbolNames[param.name] = param.span
				params.append(param.createSymbol(state))
		
		unsafe = self.unsafe
		cVarArgs = self.cVarArgs
		
		if self.cconv == CConv.C:
			unsafe = True
		elif self.cVarArgs:
			logError(state, self.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')
			cVarArgs = False
		
		self.symbol = Fn(self, params)
		return self.symbol
