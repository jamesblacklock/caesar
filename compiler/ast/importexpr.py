import os
from .ast           import AST
from ..types        import TypeSymbol
from ..sourcefile   import SourceFile
from ..not_done.mod import Mod
from ..log          import logError, logExplain
from ..             import tokenizer, parser, ir, amd64, build

ALL_IMPORTS = {}

def setExternFlags(state, mod):
	mod.extern = True
	for subMod in mod.mods:
		setExternFlags(state, subMod)
	
	for decl in (*mod.fns, *mod.statics, *mod.types):
		decl.extern = True

def flattenImportTree(imports, parentPath=None):
	newPath = [*parentPath, *imports.path] if parentPath else imports.path
	
	if imports.imports == None:
		return [ImportItem(newPath, imports.rename, imports.span)]
	
	items = []
	for tree in imports.imports:
		treeItems = flattenImportTree(tree, newPath)
		items.extend(treeItems)
	
	return items

class ImportTree(AST):
	def __init__(self, path, imports, rename, span):
		super().__init__(span)
		self.path = path
		self.imports = imports
		self.rename = rename

class ImportItem:
	def __init__(self, path, rename, span):
		self.path = path
		self.rename = rename
		self.span = span
	
	def __str__(self):
		path = '::'.join(p.content for p in self.path)
		rename = ' as {}'.format(self.rename.content) if self.rename else ''
		return path + rename

class Import(AST):
	def __init__(self, importTree, span):
		super().__init__(span)
		self.tree = importTree
		self.imports = None
		self.importedMod = None
	
	@staticmethod
	def doImport(state, mod, path, rename=None):
		class T:
			def __init__(self, content, span):
				self.content = content
				self.span = span
		
		path = [T(p, mod.span) for p in path]
		rename = T(rename, ast.span) if rename else None
		item = ImportItem(path, rename, mod.span)
		
		return Import.importItem(state, mod, item)
	
	@staticmethod
	def importItem(state, mod, item):
		importFileName = None
		symbolPath = []
		
		for i in range(0, len(item.path)):
			nameTok = item.path[i]
			name = nameTok.content
			testFileName = 'compiler/stdlib/{}.csr'.format(name)
			if os.path.exists(testFileName):
				importFileName = testFileName
				if i + 1 < len(item.path):
					symbolPath = item.path[i + 1:]
				break
		
		if importFileName == None:
			logError(state, item.span, '`{}`: could not locate the module'.format(name))
			return (None, None)
		
		if importFileName in ALL_IMPORTS:
			importedMod = ALL_IMPORTS[importFileName]
		else:
			source = SourceFile(importFileName)
			tok = tokenizer.tokenize(source)
			ast = parser.parse(source, tok)
			importedMod = state.analyze(ast)
			ir.generateIR(importedMod)
			asm = amd64.generateAsm(importedMod)
			
			asmFileName = 'compiler/stdlib/{}.asm'.format(name)
			objFileName = 'compiler/stdlib/{}.o'.format(name)
			
			try:
				outfile = open(asmFileName, 'w')
				outfile.write(asm)
				outfile.close()
				build.buildObjFile(asmFileName, objFileName)
			except Exception as e:
				print(e)
			
			importedMod.isImport = True
			importedMod.mainFn = None
			importedMod.objCodePath = objFileName
			setExternFlags(state, importedMod)
			ALL_IMPORTS[importFileName] = importedMod
		
		if importedMod not in mod.mods:
			mod.mods.append(importedMod)
		nameTok = item.path[-1]
		symbol = importedMod
		
		for tok in symbolPath:
			if tok.content == '_':
				logError(state, tok.span, '`_` is not a valid symbol name')
				return (None, None)
			elif type(symbol) == Mod or isinstance(symbol, TypeSymbol):
				symbolTok = tok
				parent = symbol
				symbol = None
				if tok.content in parent.symbolTable:
					symbol = parent.symbolTable[tok.content]
				
				if symbol == None or not symbol.pub:
					logError(state, symbolTok.span, 'cannot resolve the symbol `{}`'.format(symbolTok.content))
					return (None, None)
			else:
				logError(state, symbolTok.span, '`{}` is not a module'.format(symbol.name))
				return (None, None)
		
		nameTok = item.rename if item.rename else nameTok
		name = nameTok.content
		nameSpan = nameTok.span
		
		if name in mod.symbolTable:
			otherDecl = mod.symbolTable[symbol.name]
			logError(state, nameSpan, 'import name collides with locally defined symbol')
			logExplain(state, otherDecl.nameTok.span, '`{}` locally defined declared here'.format(name))
		else:
			symbol.nameTok = nameTok
			mod.symbolTable[name] = symbol
		
		return (importedMod, symbol)
	
	def analyzeSig(self, state, mod):
		items = flattenImportTree(self.tree)
		for item in items:
			Import.importItem(state, mod, item)
	
	def analyze(self, state, implicitType):
		pass
