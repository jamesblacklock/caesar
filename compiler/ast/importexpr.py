import os
from .ast            import AST, Name
from ..sourcefile    import SourceFile
from ..symbol.symbol import SymbolType
from ..log           import logError, logExplain
from ..              import tokenizer, parser, ir, amd64, build, platform

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
		rename = Name(rename, mod.span) if rename else None
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
			sourceFile = '{}.csr'.format(filePath)
			objectFile = '{}.o'.format(filePath)
			hasSource = os.path.exists(sourceFile)
			hasObjectCode = os.path.exists(objectFile)
			if hasSource or hasObjectCode:
				assert hasSource
				importInfo = ImportInfo()
				importInfo.id = filePath
				importInfo.sourceFile = sourceFile if hasSource else None
				importInfo.objectFile = objectFile if hasObjectCode else None
				importInfo.dir = fileDir
				importInfo.name = name
				importInfo.symbolPath = item.path[i + 1:]
				break
		
		return importInfo
	
	@staticmethod
	def importItem(state, mod, item):
		searchPaths = [
			'compiler/stdlib',
			os.path.split(mod.source.fileName)[0]
		]
		
		for path in searchPaths:
			importInfo = Import.locateImportFile(item, path)
			if importInfo:
				break
		
		if importInfo == None:
			logError(state, item.span, '`{}`: could not locate the module'.format(item.path[0].content))
			return (None, None)
		
		name = importInfo.name
		
		if importInfo.id in ALL_IMPORTS:
			importedMod = ALL_IMPORTS[importInfo.id]
		else:
			source = SourceFile(importInfo.sourceFile)
			tok = tokenizer.tokenize(source)
			ast = parser.parse(source, tok)
			
			checksum = None
			if not state.forceRebuilds:
				checksum = platform.readChecksum(importInfo)
			
			if checksum != source.checksum:
				importedMod = state.analyze(ast, isImport=True)
				ir.generateIR(importedMod)
				asm = amd64.generateAsm(importedMod)
				
				asmFileName = '{}/{}.asm'.format(importInfo.dir, name.content)
				importInfo.objectFile = '{}/{}.o'.format(importInfo.dir, name.content)
				
				try:
					outfile = open(asmFileName, 'w')
					outfile.write(asm)
					outfile.close()
					build.buildObjFile(asmFileName, importInfo.objectFile)
					os.remove(asmFileName)
				except IOError as e:
					print(e)
			else:
				importedMod = state.analyze(ast, checkOnly=True)
			
			importedMod.mainFn = None
			importedMod.objCodePath = importInfo.objectFile
			setImportFlags(state, importedMod)
			ALL_IMPORTS[importInfo.id] = importedMod
		
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
			mod.imports.append(symbol)
		
		return (importedMod, symbol)
	
	def importSymbols(self, state, mod):
		items = flattenImportTree(self.tree)
		for item in items:
			Import.importItem(state, mod, item)
	
	def analyze(self, state, implicitType):
		pass
