from .mir    import MIR, indent
from ..scope import ScopeType
from .ifexpr import If

def createDropBlock(block):
	return Block(None, False, block.span, True)

class Block(MIR):
	def __init__(self, scope, unsafe=False, span=None, isDropBlock=False):
		super().__init__(span)
		self.exprs = []
		self.scope = scope
		self.unsafe = unsafe
		self.lastIfBranchOuterSymbolInfo = None
		self.ifBranchOuterSymbolInfo = None
		self.isDropBlock = isDropBlock
	
	def append(self, mir):
		assert isinstance(mir, MIR)
		self.exprs.append(mir)
		# print(mir)
	
	def checkFlow(self, outerScope):
		if self.scope:
			scope = self.scope
			scope.didBreak = False
			scope.didReturn = False
			scope.ifBranchOuterSymbolInfo = self.ifBranchOuterSymbolInfo
		else:
			scope = outerScope
		
		for expr in self.exprs:
			assert not expr.hasValue or type(expr) == If or expr.noop
			expr.checkFlow(scope)
			if scope.didBreak or scope.didReturn:
				break
		
		if self.scope:
			if scope.type != ScopeType.FN:
				if scope.type != ScopeType.LOOP:
					outerScope.didBreak = scope.didBreak
				if scope.type not in (ScopeType.IF, ScopeType.ELSE):
					outerScope.didReturn = scope.didReturn
		
			self.propagateSymbolInfo()
	
	def propagateSymbolInfo(self):
		outerSymbolInfo = self.scope.finalize()
		
		if self.scope.type == ScopeType.IF:
			self.lastIfBranchOuterSymbolInfo = outerSymbolInfo
		elif outerSymbolInfo and self.scope.type != ScopeType.FN:
			for info in outerSymbolInfo.values():
				if info.symbol in self.scope.parent.symbolInfo:
					info.wasDeclared = self.scope.parent.symbolInfo[info.symbol].wasDeclared
				self.scope.parent.symbolInfo[info.symbol] = info
	
	def writeIR(self, state):
		state.didBreak = False
		for expr in self.exprs:
			expr.writeIR(state)
			if state.didBreak:
				break
	
	def __str__(self):
		lines = []
		totalLen = 0
		multiLine = False
		blocksOnly = True
		
		if self.isDropBlock:
			multiLine = True
			if len(self.exprs) == 0:
				return ''
		
		for expr in self.exprs:
			blocksOnly = blocksOnly and type(expr) == Block
			lineStr = str(expr)
			if lineStr == '':
				continue
			if lineStr[0] == '\n':
				lineStr = lineStr[1:]
			multiLine = multiLine or '\n' in lineStr
			totalLen += len(lineStr)
			lines.append(lineStr)
		
		if len(lines) == 0:
			return '{}'
		elif len(lines) == 1 and not self.scope:
			return lines[0]
		elif not multiLine and totalLen < 72:
			return '{{ {} }}'.format(', '.join(lines))
		
		lines = [l for l in lines if l]
		block = '\n'.join(lines)
		if not (blocksOnly):# or self.isDropBlock):
			block = indent(block)
		return '\n' + block
