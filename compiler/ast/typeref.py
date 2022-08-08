from .ast        import AST
from ..log       import logError
from ..symbol.fn import Fn
from ..types     import BUILTIN_TYPES, PtrType, ArrayType, OwnedType, USize, typesMatch
from ..mir.flow  import CFGBuilder
from ..mir.mir   import StaticDataType, GENERIC_STATIC_DATA

class TypeRef(AST):
	def __init__(self, name, span):
		super().__init__(span)
		self.name = name
	
	def resolveSig(self, state, flow=None):
		assert 0

class NamedTypeRef(TypeRef):
	def __init__(self, path, span, maybeValue=False):
		super().__init__(path[-1].content, span)
		self.path = path
		self.maybeValue = maybeValue
	
	def resolveSig(self, state, flow=None):
		builtinName = self.path[0].content if len(self.path) == 1 else None
		if builtinName in BUILTIN_TYPES:
			return BUILTIN_TYPES[builtinName]
		
		res = state.lookupSymbol(self.path, inTypePosition=True)
		return res.type if res else None

class ParamTypeRef(TypeRef):
	def __init__(self, inst, span):
		super().__init__(inst.path[-1].content, span)
		self.inst = inst
	
	def resolveSig(self, state, flow=None):
		symbol = self.inst.constructType(flow if flow else CFGBuilder(state, None, state.mod, self.span))
		if symbol:
			return symbol.type

class OwnedTypeRef(TypeRef):
	def __init__(self, baseType, acquire, release, span):
		assert (not acquire) == (not release)
		acquireStr = '' if not acquire else '::'.join(name.content for name in acquire.path)
		releaseStr = '' if not release else '::'.join(name.content for name in release.path)
		acquireRelease = '' if not acquire else '({}, {})'.format(acquireStr, releaseStr)
		name = 'owned{} {}'.format(acquireRelease, baseType.name)
		super().__init__(name, span)
		self.baseType = baseType
		self.acquire = acquire
		self.release = release
	
	def resolveSig(self, state, flow=None):
		baseType = self.baseType.resolveSig(state)
		if baseType and not baseType.isCopyable:
			logError(state, self.baseType.span, 'base type of owned type must be copyable')
		
		acquire = None
		release = None
		acquireSpan = self.acquire.span if self.acquire else self.span
		releaseSpan = self.release.span if self.release else self.span
		
		if self.acquire:
			acquire = state.lookupSymbol(self.acquire.path, inValuePosition=True)
			if acquire and type(acquire) != Fn:
				logError(state, self.acquire.span, '`{}` must be a function'.format(acquire.name))
				acquire = None
		
		if self.release:
			release = state.lookupSymbol(self.release.path, inValuePosition=True)
			if release and type(release) != Fn:
				logError(state, self.release.span, '`{}` must be a function'.format(release.name))
				release = None
		
		return OwnedType(baseType, acquire, release, acquireSpan, releaseSpan, self.span) if baseType else None

class PtrTypeRef(TypeRef):
	def __init__(self, baseType, indLevel, mut, span):
		name = '{}{}{}'.format('&' * indLevel, 'mut ' if mut else '', baseType.name)
		super().__init__(name, span)
		assert indLevel > 0
		self.baseType = baseType
		self.indLevel = indLevel
		self.mut = mut
	
	def resolveSig(self, state, flow=None):
		baseType = self.baseType.resolveSig(state)
		return PtrType(baseType, self.indLevel, self.mut) if baseType else None

class ArrayTypeRef(TypeRef):
	def __init__(self, baseType, factor, span):
		name = '[{} * <unresolved>]'.format(baseType.name)
		super().__init__(name, span)
		self.baseType = baseType
		self.factor = factor
	
	def resolveSig(self, state, flow=None):
		baseType = self.baseType.resolveSig(state)
		
		if flow == None:
			state = CFGBuilder(state, None, state.mod, self.span)
		else:
			state = flow
		
		state.beginScope(self.span)
		mir = state.analyzeNode(self.factor, USize)
		
		state.appendDropPoint()
		block = state.block
		state.endScope()
		
		count = 0
		if mir == None or mir.type == None:
			assert state.failed
		elif not typesMatch(USize, mir.type):
			logError(state, self.factor.span, 'expected type {}, found {}'.format(USize, mir.type))
		else:
			staticValue = state.staticEval(mir.symbol, [block])
			if staticValue == None:
				logError(state, self.factor.span, 'expression cannot be statically evaluated')
			elif staticValue == GENERIC_STATIC_DATA:
				count = GENERIC_STATIC_DATA
			else:
				assert staticValue.dataType == StaticDataType.INT
				count = staticValue.data
		
		self.name = '[{} * {}]'.format(baseType.name, count)
		return ArrayType(baseType, count) if baseType else None
