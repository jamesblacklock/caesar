from ..ast.ast import SymbolAST, Name
from .symbol   import SymbolType
from ..types   import typesMatch
from ..span    import Span
from ..log     import logError, logWarning, logExplain
from .trait    import Trait

class PatchMod:
	def __init__(self, parent, symbolTable):
		self.parent = parent
		self.symbolTable = symbolTable
		self.transparent = True
	
	def __getattr__(self, key):
		if key == 'parent':
			return self.parent
		elif key == 'symbolTable':
			return self.symbolTable
		elif key == 'transparent':
			return True
		return getattr(self.parent, key)

class Mod(SymbolAST):
	def __init__(self, name, doccomment, decls, span):
		super().__init__(name, span, doccomment)
		self.topLevel = False
		self.source = None
		self.decls = decls
		# self.types = []
		self.fns = []
		self.fnInsts = {}
		self.mods = []
		self.statics = []
		# self.consts = []
		self.imports = []
		self.mainFn = None
		self.isImpl = False
		self.isImport = False
		self.objCodePath = None
		self.noStrImport = False
		self.isStrMod = False
		self.acquireDefault = None
		self.releaseDefault = None
		self.acquireDefaultSet = False
		self.releaseDefaultSet = False
		self.symbols = []
		self.symbolTable = {}
		self.isFnMod = False
		self.strMod = None
		self.transparent = False
		
		self.symbolType = SymbolType.MOD
		self.isFn = False
		self.unused = True
		self.ast = self
		self.parent = None
	
	def createSymbol(self, state):
		return self
	
	def checkSig(self, state):
		from .. import attrs
		
		self.parent = state.mod
		state.mod = self
		
		if self.parent:
			self.acquireDefault = self.parent.acquireDefault
			self.releaseDefault = self.parent.releaseDefault
		
		attrs.invokeAttrs(state, self)
		
		for symbol in self.symbols:
			if symbol.isImport:
				continue
			attrs.invokeAttrs(state, symbol.ast)
		
		for symbol in self.symbols:
			if symbol.isImport:
				continue
			symbol.checkSig(state)
		
		self.mangledName = state.mangleName(self)
		state.mod = self.parent
	
	def analyze(self, state, deps):
		state.mod = self
		
		deps.push(self)
		for symbol in self.symbols:
			if symbol.isImport:
				continue
			symbol.analyze(state, deps)
		deps.pop()
		
		if not state.checkOnly:
			for fn in self.fns:
				fn.analyzeBody(state)
			
			self.fns.extend(self.fnInsts.values())
			
			for symbol in self.imports:
				isStrType = self.strMod and symbol == self.strMod.symbolTable['str']
				if symbol.unused and not isStrType:
					logWarning(state, symbol.nameSpan, 'unused import')
		
		state.mod = self.parent

class Impl(Mod):
	def __init__(self, path, traitPath, doccomment, decls, span):
		name = Name('$impl???', Span.merge(path[0].span, path[-1].span))
		super().__init__(name, doccomment, decls, span)
		self.isImpl = True
		self.path = path
		self.traitPath = traitPath
		self.trait = None
		self.type = None
		self.vtbl = None
		self.vtblName = None
	
	def checkSig(self, state):
		symbol = state.lookupSymbol(self.path, inTypePosition=True)
		if symbol:
			self.type = symbol.type
			self.name = '$impl{}'.format(state.mangleName(symbol))
		
		super().checkSig(state)
		
		if self.traitPath:
			for symbol in self.symbols:
				symbol.pub = True
			
			symbol = state.lookupSymbol(self.traitPath, inTypePosition=True)
			if symbol:
				self.trait = symbol.type
			if self.type and self.trait:
				if self.trait.isTraitType:
					if self.trait in self.type.traitImpls:
						otherImpl = self.type.traitImpls[self.trait]
						logError(state, self.traitPath[-1].span, 
							'trait `{}` already implemented for type `{}`'.format(self.trait.name, self.type.name))
						logExplain(state, otherImpl.span, 'trait `{}` previously implemented here'.format(self.trait.name))
					
					self.type.traitImpls[self.trait] = self
					
					if self.trait.isDropTrait and 'drop' in self.symbolTable:
						self.type.dropFn = self.symbolTable['drop']
						self.type.dropFn.isDropFnForType = self.type
				else:
					logError(state, self.traitPath[-1].span, '`{}` is not a trait'.format(self.trait.name))
					self.trait = None
	
	def analyze(self, state, deps):
		if not self.type:
			return
		
		if self.trait:
			traitSymbols = {symbol.name: symbol for symbol in self.trait.symbolTable.values()}
			symbolTable = {}
		else:
			traitSymbols = None
			symbolTable = self.type.symbolTable
		
		if self.type:
			for symbol in self.symbols:
				if symbol.name in symbolTable:
					otherSymbol = symbolTable[symbol.name]
					logError(state, symbol.nameSpan, 'cannot redeclare `{}` as a different symbol'.format(symbol.name))
					logExplain(state, otherSymbol.nameSpan, '`{}` previously declared here'.format(symbol.name))
					continue
				
				if traitSymbols:
					symbol.pub = True
					
					if symbol.name not in traitSymbols:
						logError(state, symbol.nameSpan, 'trait `{}` has no symbol `{}`'.format(self.trait.name, symbol.name))
					
					traitSymbol = traitSymbols[symbol.name]
					del traitSymbols[symbol.name]
					if not typesMatch(symbol.type, traitSymbol.type, selfType=self.type):
						logError(state, symbol.nameSpan, ('implementation of `{}` for trait `{}` ' + 
							'does not match the type defined by the trait').format(symbol.name, self.trait.name))
				
				symbolTable[symbol.name] = symbol
		
		if traitSymbols:
			assert 0
			logError()
			return
		
		state.mod = PatchMod(state.mod, self.type.symbolTable)
		self.parent = state.mod
		super().analyze(state, deps)
		state.mod = state.mod.parent
		
		if self.trait:
			self.vtbl = []
			self.vtblName = self.name + '__vtbl'
			if self.type.dropFn:
				self.vtbl.append(self.type.dropFn.mangledName)
			else:
				self.vtbl.append(0)
			for symbol in self.trait.mod.fns:
				implFn = symbolTable[symbol.name]
				self.vtbl.append(implFn.mangledName)

class TraitDecl(SymbolAST):
	def __init__(self, name, doccomment, pub, decls, span):
		# super().__init__(name, span, doccomment, isTraitType=True)
		super().__init__(name, span, doccomment)
		self.decls = decls
		self.isDropTrait = False
		self.pub = pub
	
	def createSymbol(self, state):
		return Trait(self)
