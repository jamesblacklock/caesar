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

def importMod(state, mod, imp):
	importFileName = None
	symbolPath = []
	
	for i in range(0, len(imp.path)):
		nameTok = imp.path[i]
		name = nameTok.content
		testFileName = 'compiler/stdlib/{}.csr'.format(name)
		if os.path.exists(testFileName):
			importFileName = testFileName
			if i + 1 < len(imp.path):
				symbolPath = imp.path[i + 1:]
			break
	
	if importFileName == None:
		logError(state, nameTok.span, '`{}`: could not locate the module'.format(name))
		return
	
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
	
	nameTok = imp.path[-1]
	symbol = importedMod
	for tok in symbolPath:
		if tok.content == '_':
			logError(state, tok.span, '`_` is not a valid symbol name')
		elif type(symbol) == Mod or isinstance(symbol, TypeSymbol):
			symbolTok = tok
			parent = symbol
			symbol = None
			if tok.content in parent.symbolTable:
				symbol = parent.symbolTable[tok.content]
			
			if symbol == None or not symbol.pub:
				logError(state, symbolTok.span, 'cannot resolve the symbol `{}`'.format(symbolTok.content))
				symbol = None
				break
		else:
			logError(state, symbolTok.span, '`{}` is not a module'.format(symbol.name))
			symbol = None
			break
	
	if symbol:
		imp.symbolImports.append(SymbolImport(symbol, imp.nameTok if imp.nameTok else nameTok))
	
	imp.importedMod = importedMod

class SymbolImport:
	def __init__(self, symbol, nameTok):
		self.name = nameTok.content
		self.symbol = symbol
		self.nameTok = nameTok

class Import(AST):
	def __init__(self, path, nameTok, span):
		self.path = path
		self.nameTok = nameTok
		self.span = span
		self.symbolImports = []
		self.importedMod = None
	
	def analyzeSig(self, state, mod):
		importMod(state, mod, self)
		if self.importedMod == None:
			return
		
		if self.importedMod not in mod.mods:
			mod.mods.append(self.importedMod)
		
		for symbolImport in self.symbolImports:
			if symbolImport.name in mod.symbolTable:
				otherDecl = mod.symbolTable[symbolImport.name]
				logError(state, symbolImport.nameTok.span, 'import name collides with locally defined symbol'.format(symbolImport.name))
				logExplain(state, otherDecl.nameTok.span, '`{}` locally defined declared here'.format(symbolImport.name))
			else:
				symbolImport.symbol.nameTok = symbolImport.nameTok
				mod.symbolTable[symbolImport.name] = symbolImport.symbol
	
	def analyze(self, state, implicitType):
		pass
