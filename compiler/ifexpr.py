from .ast   import ValueExpr
from .types import getValidAssignType, Bool, Void, OptionType
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
		
		ifExpr.block = state.analyzeNode(ifExpr.block, implicitType)
		resolvedType = ifExpr.block.type
		
		ifExpr.elseBlock.ifExpr = ifExpr
		ifExpr.elseBlock = state.analyzeNode(ifExpr.elseBlock, implicitType)
		elseResolvedType = ifExpr.elseBlock.type
		
		ifExpr.doesReturn = ifExpr.block.doesReturn and ifExpr.elseBlock.doesReturn
		
		if ifExpr.doesReturn:
			resolvedType = implicitType if implicitType else Void
		elif ifExpr.block.doesReturn:
			resolvedType = ifExpr.elseBlock.type
		elif ifExpr.elseBlock.doesReturn:
			resolvedType = ifExpr.block.type
		else:
			superType = getValidAssignType(resolvedType, elseResolvedType)
			if not superType:
				superType = getValidAssignType(elseResolvedType, resolvedType)
				if not superType:
					superType = OptionType(resolvedType, elseResolvedType)
			resolvedType = superType
		
		ifExpr.type = resolvedType
	
	def writeIR(ast, state):
		ast.expr.writeIR(state)
		
		inputTypes, inputSymbols = getInputInfo(state)
		inputTypes, inputSymbols = inputTypes[:-1], inputSymbols[:-1]
		
		ifBlock = state.defBlock(inputTypes)
		elseBlock = state.defBlock(inputTypes)
		
		state.appendInstr(BrIf(ast, ifBlock.index, elseBlock.index))
		
		beginBlock(state, ast.block, ifBlock)
		ast.block.writeIR(state)
		
		endIfBlock = None
		lastType = type(state.instr[-1])
		if lastType not in (Br, BrIf, Ret):
			endInputTypes, endInputNames = getInputInfo(state)
			endIfBlock = state.defBlock(endInputTypes)
			state.appendInstr(Br(ast, endIfBlock.index))
		
		state.setupLocals(inputTypes, inputSymbols)
		state.appendInstr(BlockMarker(ast.elseBlock, elseBlock.index))
		ast.elseBlock.writeIR(state)
		
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
		