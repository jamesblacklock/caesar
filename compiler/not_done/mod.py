from ..ast.ast  import Symbol
from ..types import TypeSymbol, typesMatch
from ..scope import ScopeType
from ..ast      import attrs
from ..log  import logError, logExplain

class Mod(Symbol):
	def __init__(self, nameTok, doccomment, decls, span, name=None):
		super().__init__(nameTok, span, doccomment)
		if name: self.name = name
		self.decls = decls
		self.types = []
		self.fns = []
		self.mods = []
		self.statics = []
		self.consts = []
		self.imports = []
		self.mainFn = None
		self.isImpl = False
		self.isImport = False
		self.objCodePath = None
		self.noStrImport = False
		self.isStrMod = False
	
	def analyzeSig(mod, state):
		state.pushScope(ScopeType.MOD, mod=mod, name=mod.name)
		
		attrs.invokeAttrs(state, mod)
		
		for decl in mod.imports:
			decl.analyzeSig(state, mod)
		
		for decl in mod.types:
			decl.analyzeSig(state)
		
		for decl in mod.fns:
			decl.analyzeSig(state)
		
		for decl in mod.statics:
			decl.analyzeSig(state)
		
		for decl in mod.consts:
			decl.analyzeSig(state)
		
		for decl in mod.mods:
			if decl.isImport:
				continue
			decl.analyzeSig(state)
		
		state.popScope()
	
	def analyze(mod, state):
		state.pushScope(ScopeType.MOD, mod=mod, name=mod.name)
		
		for decl in mod.mods:
			if decl.isImport:
				continue
			decl.analyze(state)
		
		for decl in mod.statics:
			decl.analyze(state)
		
		for decl in mod.consts:
			decl.analyze(state)
		
		for decl in mod.fns:
			decl.analyze(state)
		
		state.popScope()
	
	def pretty(self, output, indent=0):
		output.write('mod {}\n'.format(self.name), indent)
		for decl in self.decls:
			decl.pretty(output, indent + 1)
			output.write('\n\n')

IMPL_COUNTER = 0

class Impl(Mod):
	def __init__(self, path, traitPath, doccomment, decls, span):
		global IMPL_COUNTER
		name = '$impl{}'.format(IMPL_COUNTER)
		IMPL_COUNTER += 1
		super().__init__(None, doccomment, decls, span, name=name)
		self.isImpl = True
		self.path = path
		self.traitPath = traitPath
		self.trait = None
		self.type = None
		self.vtbl = None
		self.vtblName = None
	
	def analyzeSig(self, state):
		super().analyzeSig(state)
		self.type = state.lookupSymbol(self.path, inTypePosition=True)
		if self.traitPath:
			self.trait = state.lookupSymbol(self.traitPath, inTypePosition=True)
			if self.trait:
				if type(self.trait) == TraitDecl:
					if self.trait in self.type.traitImpls:
						otherImpl = self.type.traitImpls[self.trait]
						logError(state, self.traitPath[-1].span, 
							'trait `{}` already implemented for type `{}`'.format(self.trait.name, self.type.name))
						logExplain(state, otherImpl.span, 'trait `{}` previously implemented here'.format(self.trait.name))
					
					self.type.traitImpls[self.trait] = self
					
					if self.trait.isDropTrait and 'drop' in self.symbolTable:
						self.type.dropFn = self.symbolTable['drop']
				else:
					logError(state, self.traitPath[-1].span, '`{}` is not a trait'.format(self.trait.name))
					self.trait = None
	
	def analyze(self, state):
		if not self.type:
			return
		
		if self.trait:
			traitSymbols = {symbol.name: symbol for symbol in self.trait.symbolTable.values()}
			symbolTable = {}
		else:
			traitSymbols = None
			symbolTable = self.type.symbolTable
		
		if self.type:
			for decl in self.decls:
				if decl.name in symbolTable:
					otherDecl = symbolTable[decl.name]
					logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
					logExplain(state, otherDecl.nameTok.span, '`{}` previously declared here'.format(decl.name))
					continue
				
				if traitSymbols:
					decl.pub = True
					
					if decl.name not in traitSymbols:
						logError(state, decl.nameTok.span, 'trait `{}` has no symbol `{}`'.format(self.trait.name, decl.name))
					
					traitSymbol = traitSymbols[decl.name]
					del traitSymbols[decl.name]
					if not typesMatch(decl.type, traitSymbol.type, selfType=self.type):
						logError(state, decl.nameTok.span, ('implementation of `{}` for trait `{}` ' + 
							'does not match the type defined by the trait').format(decl.name, self.trait.name))
				
				symbolTable[decl.name] = decl
		
		if traitSymbols:
			assert 0
			logError()
			return
		
		super().analyze(state)
		
		if self.trait:
			self.vtbl = []
			self.vtblName = self.name + '__vtbl'
			if self.type.dropFn:
				self.vtbl.append(self.type.dropFn.mangledName)
			else:
				self.vtbl.append(0)
			for symbol in self.trait.mod.decls:
				implFn = symbolTable[symbol.name]
				self.vtbl.append(implFn.mangledName)
	
	def pretty(self, output, indent=0):
		path = '::'.join(tok.content for tok in self.path)
		output.write('impl {}\n'.format(path), indent)
		for decl in self.decls:
			decl.pretty(output, indent + 1)
			output.write('\n\n')

class TraitDecl(TypeSymbol):
	def __init__(self, nameTok, doccomment, decls, span):
		super().__init__(nameTok, span, doccomment, isTraitType=True)
		self.mod = Mod(None, None, decls, span, name='$traitmod__' + self.name)
		self.isDropTrait = False
	
	def analyzeSig(self, state):
		attrs.invokeAttrs(state, self)
		
		self.symbolTable = self.mod.symbolTable
		self.mod.analyzeSig(state)
	
	def analyze(self, state):
		self.mod.analyze(state)
	
	def pretty(self, output, indent=0):
		output.write('trait {}\n'.format(self.name), indent)
		for decl in self.decls:
			decl.pretty(output, indent + 1)
			output.write('\n\n')
