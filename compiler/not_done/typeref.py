from ..ast.ast import AST

class TypeRef(AST):
	def __init__(self, name, span):
		super().__init__(span)
		self.name = name
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)

class NamedTypeRef(TypeRef):
	def __init__(self, path, span):
		self.path = path
		self.nameTok = path[-1]
		super().__init__(self.nameTok.content, span)

class OwnedTypeRef(TypeRef):
	def __init__(self, baseType, acquire, release, span):
		assert (not acquire) == (not release)
		acquireStr = '' if not acquire else '::'.join(tok.content for tok in acquire.path)
		releaseStr = '' if not release else '::'.join(tok.content for tok in release.path)
		acquireRelease = '' if not acquire else '({}, {})'.format(acquireStr, releaseStr)
		name = 'owned{} {}'.format(acquireRelease, baseType.name)
		super().__init__(name, span)
		self.baseType = baseType
		self.acquire = acquire
		self.release = release

class PtrTypeRef(TypeRef):
	def __init__(self, baseType, indLevel, mut, span):
		name = '{}{}{}'.format('&' * indLevel, 'mut ' if mut else '', baseType.name)
		super().__init__(name, span)
		assert indLevel > 0
		self.baseType = baseType
		self.indLevel = indLevel
		self.mut = mut

class TupleTypeRef(TypeRef):
	def __init__(self, types, span):
		name = '({})'.format(', '.join(t.name for t in types))
		super().__init__(name, span)
		self.types = types

class ArrayTypeRef(TypeRef):
	def __init__(self, baseType, count, span):
		name = '[{} * {}]'.format(baseType.name, count)
		super().__init__(name, span)
		self.baseType = baseType
		self.count = count
