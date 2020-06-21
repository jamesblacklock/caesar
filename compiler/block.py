from .ast    import ValueExpr
from .types  import Void
from .       import ctlflow, letdecl, asgn, ifexpr, access
from .scope  import ScopeType
from .log    import logError, logWarning
from .span   import Span

class BlockInfo:
	def __init__(self, list, span, trailingSeparator=False):
		self.list = list
		self.span = span
		self.trailingSeparator = trailingSeparator

class Block(ValueExpr):
	def __init__(self, exprs, span, scopeType=None, noLower=False):
		super().__init__(span)
		self.exprs = exprs
		self.scopeType = scopeType
		self.doesBreak = False
		self.doesReturn = False
		self.fnDecl = None
		self.ifExpr = None
		self.loopExpr = None
		self.lowered = noLower
		self.unsafe = False
		self.scope = None
		self.scopeContracts = None
		self.lastIfBranchOuterSymbolInfo = None
		self.ifBranchOuterSymbolInfo = None
	
	@staticmethod
	def fromInfo(blockInfo, scopeType=None):
		return Block(blockInfo.list, blockInfo.span, scopeType)
	
	def analyze(block, state, implicitType):
		if block.fnDecl:
			block.unsafe = block.fnDecl.unsafe
		
		resetScopeSafety = False
		if block.scopeType:
			state.pushScope(
				block.scopeType, 
				ifExpr=block.ifExpr, 
				loopExpr=block.loopExpr,
				fnDecl=block.fnDecl, 
				allowUnsafe=block.unsafe)
			state.scope.beforeScopeLevelExpr = []
			if block.scopeContracts:
				state.scope.intersectContracts(block.scopeContracts)
			block.scope = state.scope
		elif block.unsafe and not state.scope.allowUnsafe:
			state.scope.allowUnsafe = True
			resetScopeSafety = True
		
		if block.fnDecl:
			for (i, param) in enumerate(block.fnDecl.params):
				block.fnDecl.params[i] = state.analyzeNode(param)
		
		unreachableSpan = None
		
		newExprs = []
		retVal = None
		for (i, expr) in enumerate(block.exprs):
			if block.scopeType and (state.scope.didReturn or state.scope.didBreak):
				unreachableSpan = Span.merge(unreachableSpan, expr.span) if unreachableSpan else expr.span
			
			lastExpr = i+1 == len(block.exprs)
			
			if not block.lowered:
				assert block.scopeType
				
				if expr.hasValue and type(expr) not in (Block, ifexpr.If):
					(tempSymbol, tempWrite, tempRead) = access.createTempTriple(expr)
					valueExprLowered = [tempSymbol, tempWrite]
					
					if lastExpr and implicitType != Void:
						tempWrite.rvalueImplicitType = implicitType
						valueExprLowered.append(tempRead)
					
					expr = Block(valueExprLowered, expr.span, noLower=True)
					state.scope.scopeLevelDropBlock = tempWrite.dropBlock
				elif type(expr) in (letdecl.LetDecl, asgn.Asgn):
					expr.dropBlock = Block([], expr.span)
					state.scope.scopeLevelDropBlock = expr.dropBlock
			
			if lastExpr and expr.hasValue and \
				block.scopeType == ScopeType.FN and implicitType != Void:
				retVal = expr
			else:
				expr = state.analyzeNode(expr, implicitType if lastExpr else Void)
				if type(expr) in (ctlflow.Break, ctlflow.Continue):
					expr = expr.block
				
				if block.scopeType:
					newExprs.extend(state.scope.beforeScopeLevelExpr)
					state.scope.beforeScopeLevelExpr = []
				
				newExprs.append(expr)
				
				block.doesReturn = state.scope.didReturn
				block.doesBreak = state.scope.didBreak
		
		block.exprs = newExprs
		if block.scopeType == ScopeType.FN:
			block.type = Void
			
			if retVal:
				(tempSymbol, tempWrite, tempRead) = access.createTempTriple(retVal)
				tempWrite.rvalueImplicitType = state.scope.fnDecl.returnType
				
				block.exprs.append(state.analyzeNode(tempSymbol))
				block.exprs.append(state.analyzeNode(tempWrite))
				
				if not state.scope.didReturn:
					ret = ctlflow.Return(tempRead, retVal.span)
					block.exprs.append(state.analyzeNode(ret))
			elif not state.scope.didReturn:
				ret = ctlflow.Return(retVal, retVal.span if retVal else block.span)
				block.exprs.append(state.analyzeNode(ret))
			
			block.doesReturn = state.scope.didReturn
			assert block.doesReturn
		elif len(block.exprs) == 0:
			block.type = Void
		elif block.doesReturn or block.doesBreak:
			block.type = implicitType if implicitType else Void
		else:
			lastExpr = block.exprs[-1]
			if lastExpr.hasValue:
				block.type = lastExpr.type if lastExpr.type else Void
				block.borrows = lastExpr.borrows
			else:
				block.type = Void
		
		if unreachableSpan:
			logWarning(state, unreachableSpan, 'unreachable code')
		
		if block.scopeType != None:
			state.popScope()
		elif resetScopeSafety:
			state.scope.allowUnsafe = False
	
	def accessSymbols(self, scope):
		if self.scope:
			scope = self.scope
			scope.didBreak = False
			scope.didReturn = False
			scope.ifBranchOuterSymbolInfo = self.ifBranchOuterSymbolInfo
		
		for expr in self.exprs:
			expr.accessSymbols(scope)
			if scope.didBreak or scope.didReturn:
				break
		
		if self.scope:
			self.propagateSymbolInfo()
	
	def propagateSymbolInfo(self):
		outerSymbolInfo = self.scope.finalize()
		
		if self.scope.type == ScopeType.IF:
			self.lastIfBranchOuterSymbolInfo = outerSymbolInfo
		elif self.scope.type != ScopeType.FN:
			for info in outerSymbolInfo.values():
				if info.symbol in self.scope.parent.symbolInfo:
					info.wasDeclared = self.scope.parent.symbolInfo[info.symbol].wasDeclared
				self.scope.parent.symbolInfo[info.symbol] = info
	
	def writeIR(block, state):
		state.didBreak = False
		for expr in block.exprs:
			expr.writeIR(state)
			if state.didBreak:
				break
	
	def pretty(self, output, indent=0):
		if len(self.exprs) > 0:
			if self.scopeType:
				output.write('\n')
				indent += 1
			ct = 0
			for expr in self.exprs:
				if type(expr) == Block and expr.scopeType == None and not expr.exprs:
					continue
				elif type(expr) == letdecl.FnParam:
					continue
				if ct > 0: output.write('\n')
				ct += 1
				expr.pretty(output, indent)
		elif self.scopeType:
			output.write('\n')
			output.write('{}', indent)
