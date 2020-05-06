from .ast    import ValueExpr, FnParam
from .types  import Void
from .       import ctlflow
from .       import letdecl
from .       import valueref
from .       import asgn
from .       import ifexpr
from .scope  import ScopeType
from .log    import logError
from .span   import Span

class BlockInfo:
	def __init__(self, list, span, trailingSeparator=False):
		self.list = list
		self.span = span
		self.trailingSeparator = trailingSeparator

class Block(ValueExpr):
	def __init__(self, block, scopeType=None):
		super().__init__(block.span)
		self.exprs = block.list
		self.scopeType = scopeType
		self.doesBreak = False
		self.doesReturn = False
		self.fnDecl = None
		self.ifExpr = None
		self.loopExpr = None
		self.lowered = False
	
	def lower(block, state):
		if True:#block.lowered:
			return block
		
		newExprs = []
		for (i, expr) in enumerate(block.exprs):
			lastExpr = i+1 == len(block.exprs)
			if isinstance(expr, ValueExpr) and type(expr) not in (Block, ifexpr.If):
				tempSymbol = letdecl.LetDecl(None, None, False, None, expr.span, temp=True)
				
				tempLValue = valueref.ValueRef(None, expr.span, temp=True)
				tempLValue.symbol = tempSymbol
				tempAsgn = asgn.Asgn(tempLValue, expr, expr.span, temp=True)
				tempAsgn.lowered = True
				tempAsgn.dropBlock = Block(BlockInfo([], None))
				tempAsgn.dropBlock.lowered = True
				
				newExprs.append(tempSymbol)
				newExprs.append(tempAsgn)
				newExprs.append(tempAsgn.dropBlock)
				
				if lastExpr:
					tempRef = valueref.ValueRef(None, expr.span, temp=True)
					tempRef.symbol = tempSymbol
					newExprs.append(tempRef)
			else:
				newExprs.append(expr)
		
		block.exprs = newExprs
		block.lowered = True
		return block
	
	# def handleScopeLevelDropBlock(block, state, expr):
	# 	if not isinstance(expr, ValueExpr) or block.scopeType != None or len(state.scope.dropBlock) > 0:
	# 		return expr
		
	# 	dropBlock = state.scope.dropBlock
	# 	state.scope.createNewDropBlock()
		
		
		
	# 	tempSymbol = letdecl.LetDecl(None, None, False, None, None, temp=True)
	# 	tempLValue = valueref.ValueRef(None, None, temp=True)
	# 	tempLValue.symbol = tempSymbol
	# 	tempAsgn = asgn.Asgn(tempLValue, expr, retVal.span, temp=True)
	# 	tempAsgn.lowered = True
	# 	tempAsgn.dropBlock = Block(BlockInfo([], None))
	# 	tempAsgn.dropBlock.lowered = True
		
	# 	tempSymbol = state.analyzeNode(tempSymbol)
	# 	tempAsgn = state.analyzeNode(tempAsgn)
		
	# 	if state.scope.didReturn:
	# 		block.exprs.append(tempAsgn.rvalue)
	# 	else:
	# 		tempRef = valueref.ValueRef(None, None, temp=True)
	# 		ret = ctlflow.Return(tempRef, retVal.span)
	# 		ret = state.analyzeNode(ret)
		
	
	def analyze(block, state, implicitType):
		if block.scopeType != None:
			state.pushScope(
				block.scopeType, 
				ifExpr=block.ifExpr, 
				loopExpr=block.loopExpr,
				fnDecl=block.fnDecl)
		
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
			
			if not block.lowered and isinstance(expr, ValueExpr) and type(expr) not in (Block, ifexpr.If):
				tempSymbol = letdecl.LetDecl(None, None, False, None, expr.span, temp=True)
				
				tempLValue = valueref.ValueRef(None, expr.span, temp=True)
				tempLValue.symbol = tempSymbol
				tempAsgn = asgn.Asgn(tempLValue, expr, expr.span, temp=True)
				tempAsgn.lowered = True
				tempAsgn.dropBlock = Block(BlockInfo([], None))
				tempAsgn.dropBlock.lowered = True
				
				valueExprLowered = [tempSymbol, tempAsgn, tempAsgn.dropBlock]
				
				if lastExpr and implicitType != Void:
					tempRef = valueref.ValueRef(None, expr.span, temp=True)
					tempRef.symbol = tempSymbol
					valueExprLowered.append(tempRef)
				
				if block.scopeType != None:
					state.scope.dropBlock = tempAsgn.dropBlock
				
				expr = Block(BlockInfo(valueExprLowered, expr.span))
				expr.lowered = True
			elif type(expr) == asgn.Asgn and block.scopeType != None:
				if not expr.lowered:
					assert not expr.dropBlock
					expr.dropBlock = Block(BlockInfo([], None))
					expr.dropBlock.lowered = True
				state.scope.dropBlock = expr.dropBlock
			
			if lastExpr and isinstance(expr, ValueExpr) and \
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
				tempSymbol = letdecl.LetDecl(None, None, False, None, None, temp=True)
				tempLValue = valueref.ValueRef(None, None, temp=True)
				tempLValue.symbol = tempSymbol
				tempAsgn = asgn.Asgn(tempLValue, retVal, retVal.span, temp=True)
				tempAsgn.lowered = True
				tempAsgn.dropBlock = Block(BlockInfo([], None))
				tempAsgn.dropBlock.lowered = True
				
				if block.scopeType != None:
					state.scope.dropBlock = tempAsgn.dropBlock
				
				tempSymbol = state.analyzeNode(tempSymbol)
				tempAsgn = state.analyzeNode(tempAsgn)
				
				block.exprs.append(tempSymbol)
				block.exprs.append(tempAsgn)
				block.exprs.append(tempAsgn.dropBlock)
				
				if not state.scope.didReturn:
					tempRef = valueref.ValueRef(None, None, temp=True)
					ret = ctlflow.Return(tempRef, retVal.span)
					ret = state.analyzeNode(ret)
					block.exprs.append(ret)
			elif not state.scope.didReturn:
				ret = ctlflow.Return(retVal, retVal.span if retVal else block.span)
				ret = state.analyzeNode(ret)
				block.exprs.append(ret)
			
			block.doesReturn = state.scope.didReturn
			assert block.doesReturn
		elif len(block.exprs) == 0:
			block.type = Void
		elif block.doesReturn or block.doesBreak:
			block.type = implicitType if implicitType else Void
		else:
			lastExpr = block.exprs[-1]
			if isinstance(lastExpr, ValueExpr):
				block.type = lastExpr.type
				block.borrows = lastExpr.borrows
			else:
				block.type = Void
		
		if unreachableSpan:
			logWarning(state, unreachableSpan, 'unreachable code')
		
		if block.scopeType != None:
			state.popScope()
	
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
				elif type(expr) == FnParam:
					continue
				if ct > 0: output.write('\n')
				ct += 1
				expr.pretty(output, indent)
		elif self.scopeType:
			output.write('\n')
			output.write('{}', indent)
