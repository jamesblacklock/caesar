from .ast         import ValueExpr
from ..types      import tryPromote, typesMatch, Void, Bool, OptionType
from ..scope      import ScopeType
from ..mir.ifexpr import If as IfMIR

class If(ValueExpr):
	def __init__(self, expr, ifBlock, elseBlock, span):
		super().__init__(span)
		self.expr = expr
		self.block = ifBlock
		self.elseBlock = elseBlock
	
	def analyze(self, state, implicitType):
		access = state.analyzeNode(self.expr)
		contracts = None
		if access:
			contracts = access.contracts
			if access.type and access.type != Bool:
				logError(state, self.expr.span, 
					'condition type must be bool (found {})'.format(access.type))
		
		state.pushScope(ScopeType.IF, ifExpr=self)
		state.scope.intersectContracts(contracts)
		ifAccess = state.analyzeNode(self.block, implicitType)
		ifType = Void if implicitType == Void else ifAccess.type if ifAccess.type else Void
		block = state.popScope()
		
		state.pushScope(ScopeType.ELSE, ifExpr=self)
		contracts = { c.symbol: c.inverted() for c in contracts.values() } if contracts else None
		state.scope.intersectContracts(contracts)
		elseAccess = state.analyzeNode(self.elseBlock, implicitType)
		elseType = Void if implicitType == Void else elseAccess.type if elseAccess.type else Void
		elseBlock = state.popScope()
		
		didReturn = block.scope.didReturn and elseBlock.scope.didReturn
		state.scope.didReturn = state.scope.didReturn or didReturn
		
		# didBreak = block.scope.didBreak and elseBlock.scope.didBreak
		# state.scope.didBreak = state.scope.didBreak or didBreak
		
		if implicitType == Void:
			type = Void
		elif didReturn:
			type = implicitType if implicitType else Void
		elif block.didReturn:
			type = elseType
		elif elseBlock.didReturn or typesMatch(ifType, elseType):
			type = ifType
		else:
			ifAccess = tryPromote(state, ifAccess, elseType)
			ifType = ifAccess.type
			
			elseAccess = tryPromote(state, elseAccess, ifType)
			elseType = elseAccess.type
			
			if not typesMatch(ifType, elseType):
				type = OptionType(ifType, elseType)
				ifAccess = tryPromote(state, ifAccess, ifType)
				elseAccess = tryPromote(state, elseAccess, ifType)
		
		result = None
		if type != Void:
			assert typesMatch(ifAccess.type, elseAccess.type)
			
			(tempSymbol, ifWrite, elseWrite) = SymbolAccess.createTempSymbol(ifAccess, elseAccess)
			state.analyzeNode(tempSymbol)
			state.pushMIR(block)
			state.analyzeNode(ifWrite)
			block = state.popMIR()
			state.pushMIR(elseBlock)
			state.analyzeNode(elseWrite)
			elseBlock = state.popMIR()
			
			result = SymbolRead(self.span)
			result.symbol = tempSymbol
			result.type = tempSymbol.type
			result.ref = True
		
		state.mirBlock.append(IfMIR(access, block, elseBlock, type, self.span))
		return result
