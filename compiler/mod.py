from .ast   import Symbol
from .      import attrs

class Mod(Symbol):
	def __init__(self, nameTok, doccomment, decls, span, name=None):
		super().__init__(nameTok, span, doccomment)
		if name: self.name = name
		self.mangledName = None
		self.decls = decls
		self.structs = []
		self.fns = []
		self.mods = []
		self.statics = []
		self.consts = []
		self.imports = []
		self.aliases = []
		self.mainFn = None
	
	def analyzeSig(mod, state):
		attrs.invokeAttrs(state, mod)
		
		for decl in mod.structs:
			decl.analyzeSig(state)
		
		for decl in mod.aliases:
			decl.analyzeSig(state)
		
		for decl in mod.fns:
			decl.analyzeSig(state)
		
		for decl in mod.statics:
			decl.analyzeSig(state)
		
		for decl in mod.consts:
			decl.analyzeSig(state)
		
		for decl in mod.mods:
			decl.analyzeSig(state)
	
	def analyze(mod, state, implicitType):
		for (i, decl) in enumerate(mod.mods):
			mod.mods[i] = state.analyzeNode(decl)
		
		for (i, decl) in enumerate(mod.statics):
			mod.statics[i] = state.analyzeNode(decl)
		
		for (i, decl) in enumerate(mod.consts):
			mod.consts[i] = state.analyzeNode(decl)
		
		for (i, decl) in enumerate(mod.fns):
			mod.fns[i] = state.analyzeNode(decl)
	
	def pretty(self, output, indent=0):
		output.write('mod {}\n'.format(self.name), indent)
		for decl in self.decls:
			decl.pretty(output, indent + 1)
			output.write('\n\n')
