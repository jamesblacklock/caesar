from .ast        import AST
from ..log       import logError
from ..symbol.fn import Fn
from ..types     import BUILTIN_TYPES, PtrType, ArrayType, OwnedType
from ..mir.flow  import CFGBuilder

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
		symbol = self.inst.constructType(flow if flow else CFGBuilder(state, self, state.mod))
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
	def __init__(self, baseType, count, span):
		name = '[{} * {}]'.format(baseType.name, count)
		super().__init__(name, span)
		self.baseType = baseType
		self.count = count
	
	def resolveSig(self, state, flow=None):
		baseType = self.baseType.resolveSig(state)
		return ArrayType(baseType, self.count) if baseType else None
