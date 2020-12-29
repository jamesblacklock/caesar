from .ast         import ValueSymbolAST
from ..symbol.fn  import Fn, CConv
from ..log        import logError, logExplain
from .genericinst import GenericAssocConst, GenericAssocType, GenericType

class FnDecl(ValueSymbolAST):
	def __init__(self, name, doccomment, pub, extern, cconv, unsafe, 
		genericParams, params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(name, None, span, doccomment, extern)
		self.genericParams = genericParams
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
		paramNames = {}
		genericSymbolTable = None
		genericParams = None
		if self.genericParams:
			genericParams = []
			genericSymbolTable = {}
			for param in self.genericParams:
				if param.name in paramNames:
					logError(state, param.span, 'duplicate parameter name')
					logExplain(state, paramNames[param.name], '`{}` was previously declared here'.format(param.name))
					continue
				
				if param.valueType:
					symbol = GenericAssocConst(param, None, param.span)
				else:
					symbol = GenericAssocType(param, None, param.span)
					symbol.type = GenericType(param.name.content, param.name.span, symbol)
			
			genericParams.append(param)
			genericSymbolTable[param.name.content] = symbol
			paramNames[param.name.content] = param.span
		
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
		
		self.symbol = Fn(self, params, genericParams, genericSymbolTable)
		return self.symbol
