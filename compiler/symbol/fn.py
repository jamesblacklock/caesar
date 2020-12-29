from enum       import Enum
from .symbol    import ValueSymbol
from .mod       import PatchMod
from ..log      import logError, logExplain
from ..types    import Void
from ..types    import FnType
from ..mir.flow import CFGBuilder
from ..mir.mir  import indent

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

class Fn(ValueSymbol):
	def __init__(self, ast, params, genericParams, genericSymbolTable):
		super().__init__(ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.params = params
		self.genericSymbolTable = genericSymbolTable
		self.genericParams = genericParams
		self.mangledName = None
		self.isDropFnForType = None
		self.analyzed = False
		self.extern = False
		self.isFn = True
		self.unsafe = False
		self.cVarArgs = False
		self.cfg = None
		self.inline = False
		self.genericReq = set()
		self.isGeneric = genericParams != None
	
	def checkSig(self, state):
		if self.genericSymbolTable:
			state.mod = PatchMod(state.mod, self.genericSymbolTable)
		
		for param in self.params:
			param.checkSig(state)
			if param.type and param.type.isGenericType and param.type.byteSize == None:
				self.genericReq.add(param.type.symbol)
		
		returnType = Void
		if self.ast.returnTypeRef:
			returnType = self.ast.returnTypeRef.resolveSig(state)
			if returnType and returnType.isGenericType and returnType.byteSize == None:
				self.genericReq.add(returnType.symbol)
		
		if self.genericSymbolTable:
			state.mod = state.mod.parent
		
		self.inline = self.ast.alwaysInline
		self.cVarArgs = self.ast.cVarArgs
		self.unsafe = self.ast.unsafe or self.ast.extern
		if self.ast.cconv == CConv.C:
			self.unsafe = True
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
		
		if self.genericSymbolTable:
			state.mod = PatchMod(state.mod, self.genericSymbolTable)
		
		state.finishResolvingType(self.type.returnType, deps)
		for param in self.type.params:
			state.finishResolvingType(param.type, deps)
		
		if self.genericSymbolTable:
			state.mod = state.mod.parent
		
		deps.pop()
		self.analyzed = True
	
	def analyzeBody(self, state):
		if not self.ast.body:
			return
		
		if self.genericSymbolTable:
			state.mod = PatchMod(state.mod, self.genericSymbolTable)
		
		flow = CFGBuilder(state, self, state.mod)
		flow.beginScope(self.ast.body.span, unsafe=self.unsafe)
		
		for param in self.params:
			param.dropPoint = flow.dropPoint
			flow.decl(param)
			flow.block.inputs.add(param)
		flow.appendDropPoint()
		
		self.ast.body.hasScope = False
		flow.analyzeNode(self.ast.body, self.type.returnType if self.type else None)
		
		flow.endScope()
		
		if self.genericSymbolTable:
			state.mod = state.mod.parent
		
		if not flow.failed:
			flow.finalize()
			self.cfg = flow.blocks
			self.genericReq.update(flow.genericReq)
			self.isGeneric = len(self.genericReq) > 0
			# if self.isDropFnForType:
			# 	self.checkDropFnScope(state)
		
		state.failed = state.failed or flow.failed
		
		# print(self)
		
	
	# def checkDropFnScope(self, state):
	# 	if not self.isDropFnForType.isCompositeType:
	# 		return
		
	# 	selfSymbol = self.params[0]
	# 	selfSymbolInfo = self.mirBody.scope.symbolInfo[selfSymbol]
		
	# 	mustUninit = []
	# 	for field in self.isDropFnForType.fields:
	# 		if field.type.isOwnedType:
	# 			fieldInfo = selfSymbolInfo.fieldInfo[field] if field in selfSymbolInfo.fieldInfo else None
	# 			if not fieldInfo or not fieldInfo.uninit or fieldInfo.maybeUninit:
	# 				mustUninit.append(field)
		
	# 	if mustUninit:
	# 		logError(state, self.nameSpan, '`drop` function must uninitialize all owned fields')
	# 		logExplain(state, selfSymbol.span, 'the following fields were not uninitialized: {}'.format(
	# 			', '.join('`{}`'.format(field.name) for field in mustUninit)))
	
	def __str__(self):
		fnStr = 'extern fn' if self.extern else 'fn'
		params = ', '.join(str(param) for param in self.params)
		ret = '' if self.type.returnType == Void else ' -> {}'.format(self.type.returnType)
		if self.cfg:
			body = '\n' + indent('\n'.join(str(block) for block in self.cfg))
		else:
			body = ''
		
		return '{} {}({}){}{}\n'.format(fnStr, self.name, params, ret, body)
