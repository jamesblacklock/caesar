from .ast           import AST
from ..types        import tryPromote, typesMatch, Void, Bool, OptionType
from ..mir          import access as accessmod
from ..log          import logError
from ..symbol.local import Local

class If(AST):
	def __init__(self, expr, ifBlock, elseBlock, span):
		super().__init__(span, False, True)
		self.expr = expr
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
	
	def analyze(self, state, implicitType):
		access = state.analyzeNode(self.expr)
		state.appendDropPoint()
		contracts = None
		if access:
			contracts = access.contracts
			if access.type and access.type != Bool:
				logError(state, self.expr.span, 
					'condition type must be bool (found {})'.format(access.type))
		
		(ifBranch, elseBranch) = state.ifBranch(access.symbol if access else None, self.span)
		
		self.hasValue = self.hasValue and not (implicitType and implicitType.isVoidType)
		outputSymbol = Local.createTemp(self.span) if self.hasValue else None
		
		state.beginScope(self.ifBlock.span, ifBranch)
		# state.scope.intersectContracts(contracts)
		self.ifBlock.hasScope = False
		self.ifBlock.hasValue = self.hasValue
		self.ifBlock.outputSymbol = outputSymbol
		ifAccess = state.analyzeNode(self.ifBlock, implicitType)
		endIfBranch = state.endScope()
		
		state.beginScope(self.elseBlock.span, elseBranch)
		contracts = { c.symbol: c.inverted() for c in contracts.values() } if contracts else None
		# state.scope.intersectContracts(contracts)
		self.elseBlock.hasScope = False
		self.elseBlock.hasValue = self.hasValue
		self.elseBlock.outputSymbol = outputSymbol
		elseAccess = state.analyzeNode(self.elseBlock, implicitType)
		endElseBranch = state.endScope()
		
		state.endBranch([endIfBranch, endElseBranch], self.span)
		
		if not self.hasValue:
			return None
		
		ifType = Void
		if ifAccess and ifAccess.type:
			ifType = ifAccess.type
		
		elseType = Void
		if elseAccess and elseAccess.type:
			elseType = elseAccess.type
		
		didReturn = endIfBranch.didReturn and endElseBranch.didReturn
		state.scope.didReturn = state.scope.didReturn or didReturn
		
		didBreak = endIfBranch.didBreak and endElseBranch.didBreak
		state.scope.didBreak = state.scope.didBreak or didBreak
		
		if endIfBranch.didReturn or endIfBranch.didBreak:
			return elseAccess
		else:
			return ifAccess
		
		# if implicitType and implicitType.isVoidType:
		# 	type = Void
		# elif didReturn or didBreak:
		# 	type = implicitType if implicitType else Void
		# elif endIfBranch.didReturn or endIfBranch.didBreak:
		# 	type = elseType
		# elif endElseBranch.didReturn or endElseBranch.didBreak:
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
