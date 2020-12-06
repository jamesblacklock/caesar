from .ast         import AST
from ..types      import tryPromote, typesMatch, Void, Bool, OptionType
from ..scope      import ScopeType
from ..mir.ifexpr import If as IfMIR
from ..mir.block  import createDropBlock
from ..mir        import access as accessmod
from ..log        import logError

class If(AST):
	def __init__(self, expr, ifBlock, elseBlock, span):
		super().__init__(span, True, True)
		self.expr = expr
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
	
	def analyze2(self, state, implicitType):
		# oldDropBlock = state.scope.dropBlock
		# state.scope.dropBlock = createDropBlock(self)
		access = state.analyzeNode(self.expr)
		# state.mirBlock.append(state.scope.dropBlock)
		# state.scope.dropBlock = oldDropBlock
		state.appendDropPoint()
		contracts = None
		if access:
			contracts = access.contracts
			if access.type and access.type != Bool:
				logError(state, self.expr.span, 
					'condition type must be bool (found {})'.format(access.type))
		
		(ifBranch, elseBranch) = state.ifBranch(access.symbol, self.span)
		
		# state.pushScope(ScopeType.IF)
		state.beginScope(self.ifBlock.span, ifBranch)
		# state.scope.intersectContracts(contracts)
		self.ifBlock.hasScope = False
		ifAccess = state.analyzeNode(self.ifBlock, implicitType)
		# ifBlock = state.popScope()
		endIfBranch = state.endScope()
		
		
		# state.pushScope(ScopeType.ELSE)
		state.beginScope(self.elseBlock.span, elseBranch)
		contracts = { c.symbol: c.inverted() for c in contracts.values() } if contracts else None
		# state.scope.intersectContracts(contracts)
		self.elseBlock.hasScope = False
		elseAccess = state.analyzeNode(self.elseBlock, implicitType)
		# elseBlock = state.popScope()
		endElseBranch = state.endScope()
		
		state.endBranch([endIfBranch, endElseBranch], self.span)
		
		ifType = Void
		if ifAccess and ifAccess.type:
			ifType = ifAccess.type
		
		elseType = Void
		if elseAccess and elseAccess.type:
			elseType = elseAccess.type
		
		# didReturn = ifBlock.scope.didReturn and elseBlock.scope.didReturn
		# state.scope.didReturn = state.scope.didReturn or didReturn
		
		# didBreak = ifBlock.scope.didBreak and elseBlock.scope.didBreak
		# state.scope.didBreak = state.scope.didBreak or didBreak
		
		# if implicitType == Void:
		# 	type = Void
		# elif didReturn or didBreak:
		# 	type = implicitType if implicitType else Void
		# elif ifBlock.scope.didReturn or ifBlock.scope.didBreak:
		# 	type = elseType
		# elif elseBlock.scope.didReturn or elseBlock.scope.didBreak:
		# 	type = ifType
		# elif typesMatch(ifType, elseType):
		# 	type = ifType
		# else:
		# 	type = Void
		# 	# ifAccess = tryPromote(state, ifAccess, elseType)
		# 	# ifType = ifAccess.type
			
		# 	# elseAccess = tryPromote(state, elseAccess, ifType)
		# 	# elseType = elseAccess.type
			
		# 	# if not typesMatch(ifType, elseType):
		# 	# 	type = OptionType(ifType, elseType)
		# 	# 	ifAccess = tryPromote(state, ifAccess, ifType)
		# 	# 	elseAccess = tryPromote(state, elseAccess, ifType)
		
		# result = None
		# if type != Void:
		# 	assert not (ifAccess and elseAccess) or typesMatch(ifAccess.type, elseAccess.type)
			
		# 	tempSymbol = None
		# 	if ifAccess and elseAccess:
		# 		(tempSymbol, ifWrite, elseWrite) = accessmod.createTempSymbol(ifAccess, elseAccess)
		# 		tempSymbol.declSymbol(state.scope)
		# 		state.pushMIR(ifBlock)
		# 		state.analyzeNode(ifWrite)
		# 		ifBlock = state.popMIR()
		# 		state.pushMIR(elseBlock)
		# 		state.analyzeNode(elseWrite)
		# 		elseBlock = state.popMIR()
		# 	elif ifAccess:
		# 		(tempSymbol, ifWrite) = accessmod.createTempSymbol(ifAccess)
		# 		tempSymbol.declSymbol(state.scope)
		# 		state.pushMIR(ifBlock)
		# 		state.analyzeNode(ifWrite)
		# 		ifBlock = state.popMIR()
		# 	elif elseAccess:
		# 		(tempSymbol, elseWrite) = accessmod.createTempSymbol(elseAccess)
		# 		tempSymbol.declSymbol(state.scope)
		# 		state.pushMIR(elseBlock)
		# 		state.analyzeNode(elseWrite)
		# 		elseBlock = state.popMIR()
			
		# 	if tempSymbol:
		# 		result = accessmod.SymbolRead(self.span)
		# 		result.symbol = tempSymbol
		# 		result.type = tempSymbol.type
		# 		result.ref = True
		
		# if access:
		# 	mir = IfMIR(access, ifBlock, elseBlock, type, self.span)
		# 	ifBlock.scope.ifExpr = mir
		# 	elseBlock.scope.ifExpr = mir
		# 	state.mirBlock.append(mir)
		
		# return result
