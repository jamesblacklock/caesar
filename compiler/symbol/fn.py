from enum        import Enum
from .symbol     import ValueSymbol
from ..log       import logError, logExplain
from ..types     import Void
from ..scope     import ScopeType
from ..mir.block import createDropBlock
from ..types     import FnType

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

class Fn(ValueSymbol):
	def __init__(self, ast, params):
		super().__init__(ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.params = params
		self.mangledName = None
		self.mirBody = None
		self.paramDropBlock = None
		self.isDropFnForType = None
		self.analyzed = False
		self.extern = False
		self.isFn = True
		self.unsafe = False
	
	def checkSig(self, state):
		returnType = Void
		if self.ast.returnTypeRef:
			returnType = state.resolveTypeRefSig(self.ast.returnTypeRef)
		
		for param in self.params:
			param.checkSig(state)
		
		self.cVarArgs = self.ast.cVarArgs
		self.unsafe = self.ast.unsafe
		if self.ast.cconv == CConv.C:
			unsafe = True
		elif self.ast.cVarArgs:
			logError(state, self.ast.cVarArgsSpan, 'may not use C variadic parameter without the C calling convention')
		
		self.extern = self.ast.extern
		self.type = FnType(self.unsafe, self.params, returnType, self.ast.cVarArgs, self.ast.cconv)
		self.mangledName = state.mangleName(self)
	
	def analyze(self, state, deps):
		if self.analyzed:
			return
		
		if self in deps:
			# logError(state, self.nameSpan, 'circular dependency detected')
			return
		
		deps.push(self)
		
		state.finishResolvingType(self.type.returnType, deps)
		for param in self.type.params:
			state.finishResolvingType(param.type, deps)
		
		deps.pop()
		self.analyzed = True
	
	def analyzeBody(self, state):
		if not self.ast.body:
			return
		
		state.pushScope(ScopeType.FN, self)
		
		self.paramDropBlock = createDropBlock(self)
		state.mirBlock.append(self.paramDropBlock)
		if self.type:
			for param in self.type.params:
				param.declSymbol(state.scope)
		
		alreadyFailed = state.failed
		state.failed = False    # need to know if failure occurred within this function 
		
		state.analyzeNode(self.ast.body, self.type.returnType if self.type else None)
		self.mirBody = state.popScope()
		
		if not state.failed:
			self.mirBody.checkFlow(None)
			assert self.mirBody.scope.didReturn
			# if self.isDropFnForType:
			# 	self.checkDropFnScope(state)
		
		state.failed = state.failed or alreadyFailed
		print(self)
	
	def analyzeBody2(self, state):
		if not self.ast.body:
			return
		
		state.beginFn(self)
		state.beginScope(self.ast.body.span)
		
		if self.type and self.type.params:
			# self.paramDropBlock = createDropBlock(self)
			# state.append(self.paramDropBlock)
			for param in self.type.params:
				state.decl(param)
		
		self.ast.body.hasScope = False
		state.analyzeNode2(self.ast.body, self.type.returnType if self.type else None)
		
		state.endScope()
		self.cfg = state.endFn()
		
		# if not state.failed:
		# 	self.mirBody.checkFlow(None)
		# 	assert self.mirBody.scope.didReturn
		# 	# if self.isDropFnForType:
		# 	# 	self.checkDropFnScope(state)
		
		# state.failed = state.failed or alreadyFailed
		
		self.cfg[-1].finalize()
		for block in self.cfg:
			assert block.finalized
		state.printBlocks()
		
	
	def checkDropFnScope(self, state):
		if not self.isDropFnForType.isCompositeType:
			return
		
		selfSymbol = self.params[0]
		selfSymbolInfo = self.mirBody.scope.symbolInfo[selfSymbol]
		
		mustUninit = []
		for field in self.isDropFnForType.fields:
			if field.type.isOwnedType:
				fieldInfo = selfSymbolInfo.fieldInfo[field] if field in selfSymbolInfo.fieldInfo else None
				if not fieldInfo or not fieldInfo.uninit or fieldInfo.maybeUninit:
					mustUninit.append(field)
		
		if mustUninit:
			logError(state, self.nameSpan, '`drop` function must uninitialize all owned fields')
			logExplain(state, selfSymbol.span, 'the following fields were not uninitialized: {}'.format(
				', '.join('`{}`'.format(field.name) for field in mustUninit)))
	
	def __str__(self):
		fnStr = 'extern fn' if self.extern else 'fn'
		params = ', '.join(str(param) for param in self.params)
		ret = '' if self.type.returnType == Void else ' -> {}'.format(self.type.returnType)
		body = '{}\n'.format(str(self.mirBody) if self.mirBody else '')
		
		return '{} {}({}){}{}'.format(fnStr, self.name, params, ret, body)
