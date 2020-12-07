import os
from .ast            import AST, Name
from ..sourcefile    import SourceFile
from ..symbol.symbol import SymbolType
from ..log           import logError, logExplain
from ..              import tokenizer, parser, ir, amd64, build

ALL_IMPORTS = {}

def setImportFlags(state, mod):
	mod.isImport = True
	for subMod in mod.mods:
		setImportFlags(state, subMod)
	
	for symbol in mod.symbols:
		symbol.isImport = True

def flattenImportTree(imports, parentPath=None):
	newPath = [*parentPath, *imports.path] if parentPath else imports.path
	
	if imports.imports == None:
		return [ImportItem(newPath, imports.rename, imports.span)]
	
	items = []
	for tree in imports.imports:
		treeItems = flattenImportTree(tree, newPath)
		items.extend(treeItems)
	
	return items

class ImportItem:
	def __init__(self, path, rename, span):
		self.path = path
		self.rename = rename
		self.span = span
	
	def __str__(self):
		path = '::'.join(p.content for p in self.path)
		rename = ' as {}'.format(self.rename.content) if self.rename else ''
		return path + rename

class ImportTree(AST):
	def __init__(self, path, imports, rename, span):
		super().__init__(span)
		self.path = path
		self.imports = imports
		self.rename = rename

class ImportInfo:
	pass
class Import(AST):
	def __init__(self, importTree, span):
		super().__init__(span)
		self.tree = importTree
		self.imports = None
		self.importedMod = None
	
	@staticmethod
	def doImport(state, mod, path, rename=None):
		path = [Name(p, mod.span) for p in path]
		rename = Name(rename, ast.span) if rename else None
		item = ImportItem(path, rename, mod.span)
		
		return Import.importItem(state, mod, item)
	
	@staticmethod
	def locateImportFile(item, baseDir):
		importInfo = None
		filePath = baseDir
		for i in range(0, len(item.path)):
			name = item.path[i]
			fileDir = filePath
			filePath = '{}/{}'.format(filePath, name.content)
			fileName = '{}.csr'.format(filePath)
			if os.path.exists(fileName):
				importInfo = ImportInfo()
				importInfo.fileName = fileName
				importInfo.dir = fileDir
				importInfo.name = name
				importInfo.symbolPath = item.path[i + 1:]
				break
		
		return importInfo
	
	@staticmethod
	def importItem(state, mod, item):
		searchPaths = [
			'compiler/stdlib',
			'.'
		]
		
		for path in searchPaths:
			importInfo = Import.locateImportFile(item, path)
			if importInfo:
				break
		
		if importInfo == None:
			logError(state, item.span, '`{}`: could not locate the module'.format(item.path[0].content))
			return (None, None)
		
		name = importInfo.name
		
		if importInfo.fileName in ALL_IMPORTS:
			importedMod = ALL_IMPORTS[importInfo.fileName]
		else:
			source = SourceFile(importInfo.fileName)
			tok = tokenizer.tokenize(source)
			ast = parser.parse(source, tok)
			importedMod = state.analyze(ast)
			ir.generateIR(importedMod)
			asm = amd64.generateAsm(importedMod)
			
			asmFileName = '{}/{}.asm'.format(importInfo.dir, name.content)
			objFileName = '{}/{}.o'.format(importInfo.dir, name.content)
			
			try:
				outfile = open(asmFileName, 'w')
				outfile.write(asm)
				outfile.close()
				build.buildObjFile(asmFileName, objFileName)
			except Exception as e:
				print(e)
			
			importedMod.mainFn = None
			importedMod.objCodePath = objFileName
			setImportFlags(state, importedMod)
			ALL_IMPORTS[importInfo.fileName] = importedMod
		
		if importedMod not in mod.mods:
			mod.mods.append(importedMod)
		symbol = importedMod
		
		for name in importInfo.symbolPath:
			if name.content == '_':
				logError(state, name.span, '`_` is not a valid symbol name')
				return (None, None)
			elif symbol.symbolType in (SymbolType.MOD, SymbolType.TYPE):
				parent = symbol
				symbol = None
				if name.content in parent.symbolTable:
					symbol = parent.symbolTable[name.content]
				
				if symbol == None or not symbol.pub:
					logError(state, name.span, 'cannot resolve the symbol `{}`'.format(name.content))
					return (None, None)
			else:
				logError(state, name.span, '`{}` is not a module'.format(symbol.name))
				return (None, None)
		
		name = item.rename if item.rename else item.path[-1]
		
		if name.content in mod.symbolTable:
			otherDecl = mod.symbolTable[name.content]
			logError(state, name.span, 'import name collides with locally defined symbol')
			logExplain(state, otherDecl.nameSpan, '`{}` locally defined declared here'.format(name.content))
		else:
			symbol.nameSpan = name.span
			mod.symbolTable[name.content] = symbol
		
		return (importedMod, symbol)
	
	def importSymbols(self, state, mod):
		items = flattenImportTree(self.tree)
		for item in items:
			Import.importItem(state, mod, item)
	
	def analyze(self, state, implicitType):
		pass
