from .ast    import ValueExpr
from .types  import Void
from .       import ctlflow, letdecl, asgn, ifexpr, access
from .scope  import ScopeType
from .log    import logError
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
	
	@staticmethod
	def fromInfo(blockInfo, scopeType=None):
		return Block(blockInfo.list, blockInfo.span, scopeType)
	
	def analyze(block, state, implicitType):
		if block.fnDecl:
			block.unsafe = block.fnDecl.unsafe
		
		resetScopeSafety = False
		if block.scopeType != None:
			state.pushScope(
				block.scopeType, 
				ifExpr=block.ifExpr, 
				loopExpr=block.loopExpr,
				fnDecl=block.fnDecl, 
				allowUnsafe=block.unsafe)
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
			if state.scope.didReturn or state.scope.didBreak:
				unreachableSpan = Span.merge(unreachableSpan, expr.span) if unreachableSpan else expr.span
			
			lastExpr = i+1 == len(block.exprs)
			
			if not block.lowered and expr.hasValue and type(expr) not in (Block, ifexpr.If):
				(tempSymbol, tempWrite, tempRead) = access.createTempTriple(expr)
				valueExprLowered = [tempSymbol, tempWrite]
				
				if lastExpr and implicitType != Void:
					tempWrite.rvalueImplicitType = implicitType
					valueExprLowered.append(tempRead)
				
				if block.scopeType != None:
					state.scope.dropBlock = tempWrite.dropBlock
				
				expr = Block(valueExprLowered, expr.span, noLower=True)
			
			if lastExpr and expr.hasValue and \
				block.scopeType == ScopeType.FN and implicitType != Void:
				retVal = expr
			else:
				expr = state.analyzeNode(expr, implicitType if lastExpr else Void)
				if type(expr) in (ctlflow.Break, ctlflow.Continue):
					expr = expr.block
				
				newExprs.append(expr)
				
				block.doesReturn = state.scope.didReturn
				block.doesBreak = state.scope.didBreak
		
		block.exprs = newExprs
		if block.scopeType == ScopeType.FN:
			block.type = Void
			
			if retVal:
				(tempSymbol, tempWrite, tempRead) = access.createTempTriple(retVal)
				tempWrite.rvalueImplicitType = state.scope.fnDecl.returnType
				
				if block.scopeType != None:
					state.scope.dropBlock = tempWrite.dropBlock
				
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
	
	def writeIR(block, state):
		for expr in block.exprs:
			expr.writeIR(state)
			if type(expr) in (ctlflow.Break, ctlflow.Continue, ctlflow.Return):
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
