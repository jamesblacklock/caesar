from .ast   import ValueExpr
from .types import canPromote, typesMatch, Void, Bool, OptionType
from .ir    import getInputInfo, beginBlock, Br, BrIf, Ret, BlockMarker

class If(ValueExpr):
	def __init__(self, expr, ifBlock, elseBlock, span):
		super().__init__(span)
		self.expr = expr
		self.block = ifBlock
		self.elseBlock = elseBlock
		self.doesBreak = False
		self.doesReturn = False
	
	def analyze(ifExpr, state, implicitType):
		ifExpr.expr = state.analyzeNode(ifExpr.expr)
		if ifExpr.expr.type and ifExpr.expr.type != Bool:
			logError(state, ifExpr.expr.span, 
				'condition type must be bool (found {})'.format(ifExpr.expr.type))
		
		ifExpr.block.scopeContracts = ifExpr.expr.contracts
		ifExpr.block = state.analyzeNode(ifExpr.block, implicitType)
		resolvedType = ifExpr.block.type
		
		elseContracts = None
		if ifExpr.expr.contracts:
			elseContracts = { c.symbol: c.inverted() for c in ifExpr.expr.contracts.values() }
		ifExpr.elseBlock.scopeContracts = elseContracts
		ifExpr.elseBlock.ifExpr = ifExpr
		ifExpr.elseBlock = state.analyzeNode(ifExpr.elseBlock, implicitType)
		elseResolvedType = ifExpr.elseBlock.type
		
		ifExpr.doesReturn = ifExpr.block.doesReturn and ifExpr.elseBlock.doesReturn
		state.scope.didReturn = state.scope.didReturn or ifExpr.doesReturn
		
		ifExpr.doesBreak = ifExpr.block.doesBreak and ifExpr.elseBlock.doesBreak
		state.scope.didBreak = state.scope.didBreak or ifExpr.doesBreak
		
		if ifExpr.doesReturn:
			resolvedType = implicitType if implicitType else Void
		elif ifExpr.block.doesReturn:
			resolvedType = ifExpr.elseBlock.type
		elif ifExpr.elseBlock.doesReturn:
			resolvedType = ifExpr.block.type
		elif typesMatch(resolvedType, elseResolvedType):
			pass
		elif canPromote(resolvedType, elseResolvedType):
			lastExpr = ifExpr.block[-1]
			ifExpr.block[-1] = coercion.Coercion(lastExpr, lastExpr.span, resolvedType=elseResolvedType)
			resolvedType = elseResolvedType
		elif canPromote(elseResolvedType, resolvedType):
			lastExpr = ifExpr.elseBlock[-1]
			ifExpr.elseBlock[-1] = coercion.Coercion(lastExpr, lastExpr.span, resolvedType=resolvedType)
		else:
			# ifExpr.block[-1] = ??
			# ifExpr.elseBlock[-1] = ??
			resolvedType = OptionType(resolvedType, elseResolvedType)
		
		ifExpr.type = resolvedType
	
	def accessSymbols(self, scope):
		self.expr.accessSymbols(scope)
		self.block.accessSymbols(scope)
		self.elseBlock.ifBranchOuterSymbolInfo = self.block.lastIfBranchOuterSymbolInfo
		self.elseBlock.accessSymbols(scope)
	
	def writeIR(ast, state):
		ast.expr.writeIR(state)
		
		inputTypes, inputSymbols = getInputInfo(state)
		inputTypes, inputSymbols = inputTypes[:-1], inputSymbols[:-1]
		
		ifBlock = state.defBlock(inputTypes)
		elseBlock = state.defBlock(inputTypes)
		
		state.appendInstr(BrIf(ast, ifBlock.index, elseBlock.index))
		
		beginBlock(state, ast.block, ifBlock)
		assert not state.didBreak
		ast.block.writeIR(state)
		didBreak = state.didBreak
		state.didBreak = False
		
		endIfBlock = None
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			endInputTypes, endInputNames = getInputInfo(state)
			endIfBlock = state.defBlock(endInputTypes)
			state.appendInstr(Br(ast, endIfBlock.index))
		
		state.setupLocals(inputTypes, inputSymbols)
		state.appendInstr(BlockMarker(ast.elseBlock, elseBlock.index))
		ast.elseBlock.writeIR(state)
		
		state.didBreak = state.didBreak and didBreak
		
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			if endIfBlock == None:
				endInputTypes, endInputNames = getInputInfo(state)
				endIfBlock = state.defBlock(endInputTypes)
			state.appendInstr(Br(ast, endIfBlock.index))
		
		if endIfBlock != None:
			assert not ast.doesReturn and not ast.doesBreak
			state.setupLocals(endInputTypes, endInputNames)
			beginBlock(state, ast, endIfBlock)
	
	def pretty(self, output, indent=0):
		output.write('if ', indent)
		self.expr.pretty(output)
		self.block.pretty(output, indent)
		output.write('\n')
		output.write('else', indent)
		self.elseBlock.pretty(output, indent)
		