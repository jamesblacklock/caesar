from .ast            import ValueExpr
from ..types         import Void
from .               import ifexpr
from ..mir           import access as accessmod, ctlflow
from ..scope         import ScopeType
from ..log           import logWarning
from ..span          import Span
from ..mir.primitive import VoidValue
from ..mir.block     import createDropBlock

class Block(ValueExpr):
	def __init__(self, exprs, span, scopeType=None):
		super().__init__(span)
		self.exprs = exprs
		self.scopeType = scopeType
		self.blockScope = scopeType == ScopeType.BLOCK
		self.fnDecl = None
		self.ifExpr = None
		self.loopExpr = None
		self.unsafe = False
		# self.scope = None
		self.scopeContracts = None
		self.lastIfBranchOuterSymbolInfo = None
		self.ifBranchOuterSymbolInfo = None
	
	@staticmethod
	def fromInfo(blockInfo, scopeType=None):
		return Block(blockInfo.list, blockInfo.span, scopeType)
	
	def analyze(self, state, implicitType):
		if self.fnDecl:
			self.unsafe = self.fnDecl.unsafe
		
		resetScopeSafety = False
		if self.blockScope:
			state.pushScope(ScopeType.BLOCK, allowUnsafe=self.unsafe)
		
		state.mirBlock.span = self.span
		
		unreachableSpan = None
		didReturn = False
		didBreak = False
		lastExpr = None
		access = None
		lastDropBlock = None
		state.scope.dropBlock = createDropBlock(self)
		if len(self.exprs) > 0:
			lastExpr = self.exprs.pop()
		
			for (i, expr) in enumerate(self.exprs):
				if self.scopeType and (state.scope.didReturn or state.scope.didBreak):
					unreachableSpan = Span.merge(unreachableSpan, expr.span) if unreachableSpan else expr.span
				
				if expr.hasValue and type(expr) not in (Block, ifexpr.If):
					(tempSymbol, tempWrite) = accessmod.createTempSymbol(expr)
					state.analyzeNode(tempSymbol)
					state.analyzeNode(tempWrite)
				else:
					state.analyzeNode(expr, Void)
				
				state.mirBlock.append(state.scope.dropBlock)
				state.scope.dropBlock = createDropBlock(self)
			
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
			
			lastDropBlock = None
			state.mirBlock.scope.didReturn = True
		elif access == None and implicitType != Void and state.scope.type in (ScopeType.IF, ScopeType.ELSE) and \
			not (state.scope.didReturn or state.scope.didBreak):
			(tempSymbol, tempWrite, tempRead) = accessmod.createTempTriple(VoidValue(self.span))
			state.analyzeNode(tempSymbol)
			state.analyzeNode(tempWrite)
			access = tempRead
		elif self.blockScope:
			mirBlock = state.popScope()
			if access:
				if access.type and not access.type.isVoidType:
					(tempSymbol, tempWrite, tempRead) = accessmod.createTempTriple(access)
					state.analyzeNode(tempSymbol)
					state.pushMIR(mirBlock)
					state.analyzeNode(tempWrite)
					mirBlock = state.popMIR()
					access = tempRead
				else:
					access = None
			state.mirBlock.append(mirBlock)
		
		if unreachableSpan:
			logWarning(state, unreachableSpan, 'unreachable code')
		
		if lastDropBlock:
			state.mirBlock.append(lastDropBlock)
		
		return access
