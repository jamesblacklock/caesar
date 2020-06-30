from .ast            import AST
from ..types         import Void
from .               import ifexpr
from ..mir           import access as accessmod, ctlflow
from ..scope         import ScopeType
from ..log           import logWarning
from ..span          import Span
from ..mir.primitive import VoidValue
from ..mir.block     import createDropBlock

class Block(AST):
	def __init__(self, exprs, scopeType, span):
		super().__init__(span, True)
		self.exprs = exprs
		self.scopeType = scopeType
		self.unsafe = False
	
	@staticmethod
	def fromInfo(blockInfo, scopeType):
		return Block(blockInfo.list, scopeType, blockInfo.span)
	
	def analyze(self, state, implicitType):
		resetScopeSafety = False
		if self.scopeType == ScopeType.BLOCK:
			state.pushScope(ScopeType.BLOCK, self)
		
		state.mirBlock.span = self.span
		
		unreachable = None
		didReturn = False
		didBreak = False
		lastExpr = None
		access = None
		lastDropBlock = None
		state.scope.dropBlock = createDropBlock(self)
		if len(self.exprs) > 0:
			lastExpr = self.exprs.pop()
		
			for (i, expr) in enumerate(self.exprs):
				if unreachable == None and (state.scope.didReturn or state.scope.didBreak):
					unreachable = Span.merge(expr.span, lastExpr.span)
				
				assert state.scope.dropBlock
				
				if expr.hasValue and type(expr) not in (Block, ifexpr.If):
					(tempSymbol, tempWrite) = accessmod.createTempSymbol(expr)
					state.analyzeNode(tempSymbol)
					state.analyzeNode(tempWrite)
				else:
					state.analyzeNode(expr, Void)
				
				state.mirBlock.append(state.scope.dropBlock)
				state.scope.dropBlock = createDropBlock(self)
			
			if unreachable == None and (state.scope.didReturn or state.scope.didBreak):
				unreachable = lastExpr.span
			access = state.analyzeNode(lastExpr, implicitType)
		
		lastDropBlock = state.scope.dropBlock
		state.scope.dropBlock = None
		
		if state.scope.type == ScopeType.FN:
			if access and implicitType != Void:
				assert not state.scope.didReturn
				ret = ctlflow.Return(access, lastDropBlock, access.span)
				state.mirBlock.append(lastDropBlock)
				state.analyzeNode(ret)
				access.dropBlock = lastDropBlock
			elif not state.scope.didReturn:
				ret = ctlflow.Return(None, lastDropBlock, self.span)
				state.mirBlock.append(lastDropBlock)
				state.analyzeNode(ret)
				access = None
			
			lastDropBlock = None
		elif access == None and implicitType != Void and state.scope.type in (ScopeType.IF, ScopeType.ELSE) and \
			not (state.scope.didReturn or state.scope.didBreak):
			(tempSymbol, tempWrite, tempRead) = accessmod.createTempTriple(VoidValue(self.span))
			state.analyzeNode(tempSymbol)
			state.analyzeNode(tempWrite)
			tempRead.type = tempSymbol.type
			access = tempRead
		elif self.scopeType == ScopeType.BLOCK:
			mirBlock = state.popScope()
			if access:
				if access.type and not access.type.isVoidType:
					(tempSymbol, tempWrite, tempRead) = accessmod.createTempTriple(access)
					state.analyzeNode(tempSymbol)
					state.pushMIR(mirBlock)
					state.analyzeNode(tempWrite)
					mirBlock = state.popMIR()
					tempRead.type = tempSymbol.type
					access = tempRead
				else:
					access = None
			state.mirBlock.append(mirBlock)
		
		if unreachable:
			logWarning(state, unreachable, 'unreachable code')
		
		if lastDropBlock:
			state.mirBlock.append(lastDropBlock)
		
		return access
