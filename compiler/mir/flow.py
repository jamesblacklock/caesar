from ..span  import Span
from .cfg    import CFGBlock, CFGDropPoint
from .access import SymbolAccess, SymbolRead

class Scope:
	def __init__(self, parent, span, loop, branch, unsafe, contracts):
		self.parent = parent
		self.symbols = set(parent.symbols) if parent else set()
		self.symbolTable = dict(parent.symbolTable) if parent else {}
		self.declaredSymbols = set()
		self.loop = loop
		self.branch = branch
		self.didBreak = False
		self.didReturn = False
		self.span = span
		self.allowUnsafe = unsafe or parent and parent.allowUnsafe
		self.contracts = dict(parent.contracts) if parent else {}
		if contracts:
			self.intersectContracts(contracts)
	
	def intersectContracts(self, contracts):
		if contracts == None:
			return
		
		for contract in contracts.values():
			if contract.symbol in self.contracts:
				self.contracts[contract.symbol] = self.contracts[contract.symbol].intersect(contract)
			else:
				self.contracts[contract.symbol] = contract

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
		self.blockId = 1
		self.dropPoint = None
	
	def createDiscardBuilder(self):
		discard = CFGBuilder(self.ssstate, self.fn, self.mod)
		discard.block = self.block.cloneForDiscard()
		discard.blocks = [discard.block]
		discard.scope = self.scope
		discard.breakBlocks = [b.cloneForDiscard() for b in self.breakBlocks]
		discard.continueBlocks = [b.cloneForDiscard() for b in self.continueBlocks]
		discard.branchPoints = [b.cloneForDiscard() for b in self.branchPoints]
		discard.endSpan = self.endSpan
		discard.failed = self.failed
		discard.blockId = self.blockId
		discard.dropPoint = CFGDropPoint(discard.block.span)
		return discard
	
	def appendBlock(self, block):
		self.blocks.append(block)
		block.id = len(self.blocks)
	
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
		self.appendBlock(self.block)
	
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
	
	def beginScope(self, span, branch=None, loop=False, unsafe=False, contracts=None):
		lastBlock = self.block
		
		if branch:
			if self.block:
				self.block.span = Span.merge(self.block.span.startSpan(), span.startSpan())
			self.block = branch
			branch.span = span
			self.appendBlock(branch)
		else:
			self.beginBlock(span)
		
		if loop:
			self.continueBlocks.append(self.block)
			self.breakBlocks.append(CFGBlock([], Span.merge(span.endSpan(), self.endSpan)))
		
		self.scope = Scope(self.scope, span, loop, branch != None, unsafe, contracts)
		
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
			self.appendBlock(self.block)
		else:
			self.beginBlock(self.scope.span.endSpan())
		
		if self.scope.didBreak and not (self.scope.parent.loop or self.scope.parent.branch):
			self.scope.parent.didBreak = True
		self.scope = self.scope.parent
		
		return endBlock
	
	def analyzeNode(self, ast, implicitType=None, isRValue=False, discard=False):
		from ..attrs import invokeAttrs
		invokeAttrs(self, ast)
		
		flow = self
		if discard:
			flow = flow.createDiscardBuilder()
		
		access = ast.analyze(flow, implicitType)
		if access:
			assert access.hasValue
			if type(access) != SymbolRead and not isRValue:
				access = SymbolAccess.read(flow, access)
		
		return access
	
	def decl(self, symbol):
		self.scope.symbolTable[symbol.name] = symbol
		self.scope.symbols.add(symbol)
		self.scope.declaredSymbols.add(symbol)
		self.block.decl(symbol)
	
	def access(self, access):
		self.block.access(self, access)
	
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
		
		self.blocks = blocks
		self.blocks[0].finalizeInputs(self)
		
		for block in self.blocks:
			block.finalize(self)
	
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