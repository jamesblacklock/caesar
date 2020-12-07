from ..span  import Span
from .cfg    import CFGBlock, CFGDropPoint
from .access import SymbolAccess, SymbolRead

class Scope:
	def __init__(self, parent, span, loop=False, branch=False, unsafe=False):
		self.parent = parent
		self.symbols = list(parent.symbols) if parent else []
		self.symbolTable = dict(parent.symbolTable) if parent else {}
		self.declaredSymbols = []
		self.loop = loop
		self.branch = branch
		self.didBreak = False
		self.didReturn = False
		self.span = span
		self.allowUnsafe = unsafe or parent and parent.allowUnsafe

class CFGBuilder:
	def __init__(self, state, fn, mod):
		self.ssstate = state
		self.mod = mod
		self.fn = fn
		self.block = None
		self.scope = None
		self.blocks = []
		self.breakBlocks = []
		self.continueBlocks = []
		self.branchPoints = []
		self.endSpan = fn.span.endSpan()
		self.failed = False
	
	def ifBranch(self, branchOn, span):
		self.block.branchOn = branchOn
		ifBranch = CFGBlock([self.block], span)
		elseBranch = CFGBlock([self.block], span)
		self.block.successors = [elseBranch, ifBranch]
		self.branchPoints.append(self.block)
		return (ifBranch, elseBranch)
	
	def endBranch(self, branches, span):
		branchPoint = self.branchPoints.pop()
		didBreak = True
		didReturn = True
		for b in branches:
			didBreak = didBreak and b.didBreak
			didReturn = didReturn and b.didReturn
		
		branches = [b for b in branches if not (b.didBreak or b.didReturn)]
		if branches:
			self.beginBlock(span.endSpan(), branches)
		elif didBreak:
			self.block = self.breakBlocks[-1]
		else:
			self.beginBlock(span.endSpan(), None)
			self.block.symbolState.update(branchPoint.symbolState)
	
	def beginBlock(self, span, ancestors=None):
		if self.block:
			ancestors = [self.block]
			self.block.span = Span.merge(self.block.span.startSpan(), span.startSpan())
		blockSpan = Span.merge(span.startSpan(), self.endSpan)
		self.block = CFGBlock(ancestors, blockSpan)
		self.dropPoint = CFGDropPoint(blockSpan)
		self.blocks.append(self.block)
	
	def doBreak(self):
		self.scope.didBreak = True
		self.block.didBreak = True
		self.block.successors = []
		self.breakBlocks[-1].addAncestor(self.block)
	
	def doContinue(self):
		self.scope.didBreak = True
		self.block.didBreak = True
		self.block.successors = []
		self.continueBlocks[-1].addReverseAncestor(self.block)
	
	def beginScope(self, span, branch=None, loop=False, unsafe=False):
		lastBlock = self.block
		
		if branch:
			if self.block:
				self.block.span = Span.merge(self.block.span.startSpan(), span.startSpan())
			self.block = branch
			branch.span = span
			self.blocks.append(branch)
		else:
			self.beginBlock(span)
		
		if loop:
			self.continueBlocks.append(self.block)
			self.breakBlocks.append(CFGBlock([], Span.merge(span.endSpan(), self.endSpan)))
		
		self.scope = Scope(self.scope, span, loop, branch != None, unsafe)
		
		return self.block
	
	def endScope(self):
		endBlock = self.block
		
		if self.scope.parent == None or self.scope.branch:
			self.block = None
		elif self.scope.loop:
			startBlock = self.continueBlocks.pop()
			if not self.scope.didBreak and self.block != self.breakBlocks[-1]:
				startBlock.addReverseAncestor(self.block)
			self.block.span = Span.merge(self.block.span.startSpan(), self.scope.span.endSpan())
			self.block = self.breakBlocks.pop()
			self.blocks.append(self.block)
		else:
			self.beginBlock(self.scope.span.endSpan())
		
		if self.scope.didBreak and not (self.scope.parent.loop or self.scope.parent.branch):
			self.scope.parent.didBreak = True
		self.scope = self.scope.parent
		
		return endBlock
	
	def analyzeNode(self, ast, implicitType=None, isRValue=False, discard=False):
		from ..attrs import invokeAttrs
		invokeAttrs(self, ast)
		
		blocks = self.blocks
		if discard:
			self.blocks = []
		
		access = ast.analyze(self, implicitType)
		if access:
			assert access.hasValue
			if type(access) != SymbolRead and not isRValue:
				access = SymbolAccess.read(self, access)
		
		self.blocks = blocks
		return access
	
	def decl(self, symbol):
		self.scope.symbolTable[symbol.name] = symbol
		self.scope.symbols.append(symbol)
		self.scope.declaredSymbols.append(symbol)
		self.block.decl(symbol)
	
	def access(self, access):
		self.block.access(access)
	
	def append(self, mir):
		mir.commit(self)
		self.block.append(mir)
	
	def appendDropPoint(self):
		self.block.append(self.dropPoint)
		self.dropPoint = CFGDropPoint(self.block.span)
	
	def finalize(self):
		blocks = []
		for block in self.blocks:
			if block.id != 1:
				unreachable = True
				for ancestor in block.ancestors:
					if not ancestor.unreachable:
						unreachable = False
						break
				if unreachable:
					block.unreachable = True
					continue
			blocks.append(block)
			if not block.successors:
				block.finalize()
		
		self.blocks = blocks
		for block in self.blocks:
			assert block.finalized
	
	def lookupSymbol(self, path, symbolTable=None, inTypePosition=False, inValuePosition=False):
		symbolTable = {**symbolTable, **self.scope.symbolTable} if symbolTable else self.scope.symbolTable
		return self.ssstate.lookupSymbol(path, symbolTable, inTypePosition, inValuePosition)
	
	def resolveTypeRef(self, typeRef):
		return self.ssstate.resolveTypeRef(typeRef)
	
	def typeCheck(self, expr, expectedType):
		return self.ssstate.typeCheck(expr, expectedType)
	
	def printBlocks(self, block=None):
		if self.blocks:
			for block in self.blocks:
				block.span.reveal()
				if block.finalized:
					print('\033[32;1m')
				print(block)
				print('\033[0m')