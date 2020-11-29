from .ast            import AST
from ..types         import Void
from .               import ifexpr
from ..mir           import access as accessmod, ctlflow
from ..mir.access    import createTempSymbol
from ..scope         import ScopeType
from ..log           import logWarning
from ..span          import Span
from ..mir.primitive import VoidValue
from ..mir.block     import createDropBlock

class Block(AST):
	def __init__(self, exprs, scopeType, span):
		super().__init__(span, True, True)
		self.exprs = exprs
		self.scopeType = scopeType
		self.unsafe = False
	
	@staticmethod
	def fromInfo(blockInfo, scopeType):
		return Block(blockInfo.list, scopeType, blockInfo.span)
	
	def analyze(self, state, implicitType):
		if self.scopeType == ScopeType.BLOCK:
			state.pushScope(ScopeType.BLOCK, self)
		
		state.mirBlock.span = self.span
		
		unreachable = None
		lastExpr = None
		access = None
		lastDropBlock = None
		state.scope.dropBlock = createDropBlock(self)
		if len(self.exprs) > 0:
			lastExpr = self.exprs.pop()
		
			for (i, expr) in enumerate(self.exprs):
				if unreachable == None and (state.scope.didReturn or state.scope.didBreak):
					unreachable = Span.merge(expr.span, lastExpr.span)
				
				if expr.hasValue and not expr.hasBlockValue:
					(tempSymbol, tempWrite) = accessmod.createTempSymbol(expr)
					tempSymbol.declSymbol(state.scope)
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
			# need to create a return
			if not state.scope.didReturn:
				if access and implicitType and not implicitType.isVoidType:
					ret = ctlflow.Return(access, lastDropBlock, access.span)
					state.mirBlock.append(lastDropBlock)
					state.analyzeNode(ret)
					access.dropBlock = lastDropBlock
				else:
					if access and implicitType and not access.type.isVoidType:
						(tempSymbol, tempWrite) = accessmod.createTempSymbol(access)
						tempSymbol.declSymbol(state.scope)
						state.scope.dropBlock = lastDropBlock
						state.analyzeNode(tempWrite)
						state.scope.dropBlock = None
					ret = ctlflow.Return(None, lastDropBlock, self.span)
					state.mirBlock.append(lastDropBlock)
					state.analyzeNode(ret)
			
			access = None
			lastDropBlock = None
		elif state.scope.type in (ScopeType.IF, ScopeType.ELSE):
			# need to create a result
			if not access and implicitType != Void and not (state.scope.didReturn or state.scope.didBreak):
				(tempSymbol, tempWrite, tempRead) = accessmod.createTempTriple(VoidValue(self.span))
				tempSymbol.declSymbol(state.scope)
				state.analyzeNode(tempWrite)
				tempRead.type = Void
				access = tempRead
		elif self.scopeType == ScopeType.BLOCK:
			mirBlock = state.popScope()
			if access:
				# need to prepend a symbol before the block to write the result to
				if access.type and not access.type.isVoidType:
					(tempSymbol, tempWrite, tempRead) = accessmod.createTempTriple(access)
					tempSymbol.declSymbol(state.scope)
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
