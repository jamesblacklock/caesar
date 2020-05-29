from .ast   import Symbol
from .scope import ScopeType
from .      import attrs

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
	
	def analyzeSig(mod, state):
		state.pushScope(ScopeType.MOD, mod=mod, name=mod.name)
		
		attrs.invokeAttrs(state, mod)
		
		for decl in mod.types:
			decl.analyzeSig(state)
		
		for decl in mod.fns:
			decl.analyzeSig(state)
		
		for decl in mod.statics:
			decl.analyzeSig(state)
		
		for decl in mod.consts:
			decl.analyzeSig(state)
		
		for decl in mod.mods:
			decl.analyzeSig(state)
		
		state.popScope()
	
	def analyze(mod, state, implicitType):
		state.pushScope(ScopeType.MOD, mod=mod, name=mod.name)
		
		decls = []
		for (i, decl) in enumerate(mod.mods):
			decl = state.analyzeNode(decl)
			mod.mods[i] = decl
			decls.append(decl)
		
		for (i, decl) in enumerate(mod.statics):
			decl = state.analyzeNode(decl)
			mod.statics[i] = decl
			decls.append(decl)
		
		for (i, decl) in enumerate(mod.consts):
			decl = state.analyzeNode(decl)
			mod.consts[i] = decl
			decls.append(decl)
		
		for (i, decl) in enumerate(mod.fns):
			decl = state.analyzeNode(decl)
			mod.fns[i] = decl
			decls.append(decl)
		
		mod.decls = decls
		
		state.popScope()
	
	def pretty(self, output, indent=0):
		output.write('mod {}\n'.format(self.name), indent)
		for decl in self.decls:
			decl.pretty(output, indent + 1)
			output.write('\n\n')

IMPL_COUNTER = 0

class Impl(Mod):
	def __init__(self, path, doccomment, decls, span):
		global IMPL_COUNTER
		name = '$impl{}'.format(IMPL_COUNTER)
		IMPL_COUNTER += 1
		super().__init__(None, doccomment, decls, span, name=name)
		self.path = path
		self.type = None
	
	def analyzeSig(self, state):
		super().analyzeSig(state)
		self.type = state.lookupSymbol(self, True)
	
	def analyze(self, state, implicitType):
		super().analyze(state, implicitType)
		
		if self.type:
			for decl in self.decls:
				if decl.name in self.type.symbolTable:
					otherDecl = self.type.symbolTable[decl.name]
					logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
					logExplain(state, otherDecl.nameTok.span, '`{}` previously declared here'.format(decl.name))
				else:
					self.type.symbolTable[decl.name] = decl
			
	
	def pretty(self, output, indent=0):
		output.write('mod {}\n'.format(self.name), indent)
		for decl in self.decls:
			decl.pretty(output, indent + 1)
			output.write('\n\n')
