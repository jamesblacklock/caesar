from enum     import Enum
from ..ast.ast    import ValueSymbol#, TypeModifiers
from ..log    import logError, logExplain
from ..types  import FnType, Void
from ..   import scope
from ..mir.block import createDropBlock

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

class FnDecl(ValueSymbol):
	def __init__(self, nameTok, doccomment, pub, extern, cconv, unsafe, 
		params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(nameTok, None, span, doccomment, extern)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cVarArgsSpan = cVarArgsSpan
		self.returnTypeRef = returnType
		self.returnType = None
		# self.returnTypeModifiers = TypeModifiers()
		self.body = body
		self.cconv = cconv
		self.unsafe = unsafe
		self.pub = pub
		self.mirBody = None
		self.paramDropBlock = None
		self.isDropFnForType = None
	
	def analyzeSig(self, state):
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
		
		if self.cconv == CConv.C:
			self.unsafe = True
		elif self.cVarArgs:
			logError(state, self.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')
		
		self.mangledName = state.mangleName(self)
	
	def analyze(self, state):
		self.returnType = state.finishResolvingType(self.returnType)
		for param in self.params:
			param.type = state.finishResolvingType(param.type)
		
		state.pushScope(scope.ScopeType.FN, self)
		
		self.paramDropBlock = createDropBlock(self)
		state.mirBlock.append(self.paramDropBlock)
		for param in self.params:
			state.analyzeNode(param)
		
		if self.extern:
			state.popScope()
			return
		
		state.analyzeNode(self.body, self.returnType)
		self.mirBody = state.popScope()
		
		if not state.failed:
			self.mirBody.checkFlow(None)
			assert self.mirBody.scope.didReturn
			# if self.isDropFnForType:
			# 	self.checkDropFnScope(state)
		
		# print(self)
	
	def checkDropFnScope(self, state):
		if not self.isDropFnForType.isCompositeType:
			return
		
		selfSymbol = self.params[0].symbol
		selfSymbolInfo = self.mirBody.scope.symbolInfo[selfSymbol]
		
		mustUninit = []
		for field in self.isDropFnForType.fields:
			if field.type.isOwnedType:
				fieldInfo = selfSymbolInfo.fieldInfo[field] if field in selfSymbolInfo.fieldInfo else None
				if not fieldInfo or not fieldInfo.uninit or fieldInfo.maybeUninit:
					mustUninit.append(field)
		
		if mustUninit:
			logError(state, self.nameTok.span, '`drop` function must uninitialize all owned fields')
			logExplain(state, selfSymbol.span, 'the following fields were not uninitialized: {}'.format(
				', '.join('`{}`'.format(field.name) for field in mustUninit)))
	
	def __str__(self):
		fnStr = 'extern fn' if self.extern else 'fn'
		params = ', '.join(str(param.symbol) for param in self.params)
		ret = '' if self.returnType == Void else ' -> {}'.format(self.returnType)
		body = '{}\n'.format(str(self.mirBody) if self.mirBody else '')
		
		return '{} {}({}){}{}'.format(fnStr, self.name, params, ret, body)
