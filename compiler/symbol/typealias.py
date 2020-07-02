from ..symbol.symbol import Symbol, SymbolType

class TypeAlias(Symbol):
	def __init__(self, ast):
		super().__init__(SymbolType.TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		self.ast = ast
		self.analyzed = False
	
	def checkSig(self, state):
		self.type = state.resolveTypeRefSig(self.ast.typeRef)
		return
	
	def analyze(self, state, deps):
		if self.analyzed:
			return
		
		if self in deps:
			# logError(state, self.nameSpan, 'circular dependency detected')
			return
		
		deps.push(self)
		state.finishResolvingType(self.type, deps)
		deps.pop()
		self.analyzed = True
	
	# def analyzeSig(self, state):
	# 	self.baseType = state.resolveTypeRefSig(self.typeRef)
	# 	self.byteSize = self.baseType.byteSize
	# 	self.align = self.baseType.align
	
	# def pretty(self, output, indent=0):
	# 	output.write('type ', indent)
	# 	output.write(self.name)
	# 	output.write(' = ')
	# 	if self.baseType:
	# 		output.write(self.baseType.name)
	# 	else:
	# 		output.write(self.typeRef.name)
