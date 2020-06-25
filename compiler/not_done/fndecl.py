from enum     import Enum
from ..ast.ast    import ValueSymbol#, TypeModifiers
from ..log    import logError
from ..types  import FnType, Void
from ..ast        import attrs
from ..   import scope

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

class FnDecl(ValueSymbol):
	def __init__(self, nameTok, doccomment, pub, extern, unsafe, 
		params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(nameTok, None, span, doccomment, extern)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cVarArgsSpan = cVarArgsSpan
		self.returnTypeRef = returnType
		self.returnType = None
		# self.returnTypeModifiers = TypeModifiers()
		self.body = body
		self.cconv = CConv.CAESAR
		self.unsafe = unsafe
		self.pub = pub
		self.mirBody = None
	
	def analyzeSig(self, state):
		attrs.invokeAttrs(state, self)
		
		if self.returnTypeRef:
			self.returnType = state.resolveTypeRefSig(self.returnTypeRef)
		else:
			self.returnType = Void
		
		symbolNames = set()
		for param in self.params:
			param.type = state.resolveTypeRefSig(param.typeRef)
			if param.name in symbolNames:
				logError(state, param.span, 'duplicate parameter name')
			else:
				symbolNames.add(param.name)
		
		self.type = FnType(self.unsafe, self.params, self.returnType, self.cVarArgs, self.cconv)
		
		if self.cVarArgs:
			if self.cconv != CConv.C:
				logError(state, self.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')
		
		if self.extern and not self.unsafe:
			logError(state, self.nameTok.span, '`{}` must be labelled `unsafe` because it is `extern`'.format(self.name))
		
		self.mangledName = state.mangleName(self)
	
	def analyze(self, state):
		self.returnType = state.finishResolvingType(self.returnType)
		for param in self.params:
			param.type = state.finishResolvingType(param.type)
		
		state.pushScope(scope.ScopeType.FN, fnDecl=self)
		
		for param in self.params:
			state.analyzeNode(param)
		
		if self.extern:
			state.popScope()
			return
		
		state.analyzeNode(self.body, self.returnType)
		self.mirBody = state.popScope()
		
		# print(self)
		
		if not state.failed:
			self.mirBody.checkFlow(None)
			assert self.mirBody.scope.didReturn
	
	def __str__(self):
		fnStr = 'extern fn' if self.extern else 'fn'
		params = ', '.join(str(param.symbol) for param in self.params)
		ret = '' if self.returnType == Void else ' -> {}'.format(self.returnType)
		body = '{}\n'.format(str(self.mirBody) if self.mirBody else '')
		
		return '{} {}({}){}{}'.format(fnStr, self.name, params, ret, body)
