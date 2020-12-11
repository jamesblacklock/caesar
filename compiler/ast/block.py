from .ast            import AST
from ..types         import Void
from .ctlflow        import Return
from ..mir           import access as accessmod
from ..mir.access    import createTempSymbol
from ..log           import logWarning
from ..span          import Span
from ..mir.primitive import VoidValue

class Block(AST):
	def __init__(self, exprs, span):
		super().__init__(span, False, True)
		self.exprs = exprs
		self.unsafe = False
		self.hasScope = True
		self.outputSymbol = None
	
	@staticmethod
	def fromInfo(blockInfo):
		return Block(blockInfo.list, blockInfo.span)
	
	def analyze(self, state, implicitType):
		unreachable = None
		lastExpr = None
		access = None
		exprs = list(self.exprs)
		
		if self.hasScope:
			state.beginScope(self.span, unsafe=self.unsafe)
		
		if len(exprs) > 0:
			lastExpr = exprs.pop()
		
			for (i, expr) in enumerate(exprs):
				if unreachable == None and (state.scope.didReturn or state.scope.didBreak):
					unreachable = Span.merge(expr.span, lastExpr.span)
				
				if expr.hasValue and not expr.hasBlockValue:
					(tempSymbol, tempWrite) = accessmod.createTempSymbol(expr)
					state.decl(tempSymbol)
					state.analyzeNode(tempWrite)
				else:
					state.analyzeNode(expr, Void)
				
				state.appendDropPoint()
			
			if unreachable == None and (state.scope.didReturn or state.scope.didBreak):
				unreachable = lastExpr.span
			if state.scope.parent == None and not state.block.didReturn and type(lastExpr) != Return:
				self.hasValue = implicitType and not implicitType.isVoidType
			if lastExpr.hasBlockValue:
				lastExpr.hasValue = self.hasValue
			access = state.analyzeNode(lastExpr, implicitType)
			state.appendDropPoint()
		
		if access and (access.deref or not access.ref or not self.hasValue):
			(symbol, write, access) = accessmod.createTempTriple(access)
			state.decl(symbol)
			state.analyzeNode(write)
			state.appendDropPoint()
		
		if self.hasValue and not (state.scope.didBreak or state.scope.didReturn):
			if access == None:
				(symbol, write, access) = accessmod.createTempTriple(VoidValue(self.span))
				state.decl(symbol)
				state.analyzeNode(write)
				state.appendDropPoint()
			
			if self.outputSymbol:
				state.decl(self.outputSymbol)
				access = accessmod.SymbolAccess.write(state, self.outputSymbol, access)
				state.appendDropPoint()
			else:
				assert access.ref
				self.outputSymbol = access.symbol
			
			state.block.outputs.add(self.outputSymbol)
		else:
			access = None
		
		if state.scope.parent == None:
			state.block.didReturn = True
			state.block.returnAccess = access
		
		if unreachable:
			logWarning(state, unreachable, 'unreachable code')
		
		if self.hasScope:
			state.endScope()
		
		return access
