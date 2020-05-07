from .ast      import ValueExpr
from .         import fndecl, letdecl, staticdecl, block, asgn
from .coercion import Coercion
from .types    import canPromote
from .ir       import IPTR, Dup, Raise, Global, Deref

class ValueRef(ValueExpr):
	def __init__(self, path, span, name=None, tempSymbol=None):
		super().__init__(span)
		self.path = path
		self.symbol = tempSymbol
		self.copy = False
		self.write = False
		self.deref = False
		self.addr = False
		self.fieldAccess = False
		self.nameTok = path[-1] if path else None
		self.temp = tempSymbol != None
		if path:
			self.name = self.nameTok.content
		elif tempSymbol:
			self.name = tempSymbol.name
		else:
			assert name
			self.name = name
		self.dropBlock = None
	
	@staticmethod
	def createTemp(tempSymbol):
		return ValueRef(None, tempSymbol.span, tempSymbol=tempSymbol)
	
	def lower(valueRef, state):
		if valueRef.write or valueRef.dropBlock:
			return valueRef
		
		if valueRef.symbol == None:
			valueRef.symbol = state.lookupSymbol(valueRef)
			if valueRef.symbol == None:
				return valueRef
		
		if type(valueRef.symbol) != letdecl.LetDecl or not valueRef.symbol.dropFn:
			return valueRef
		
		(tempSymbol, tempAsgn, tempRef) = letdecl.createTempTriple(valueRef)
		
		valueRef.dropBlock = block.Block([], valueRef.span)
		valueRef.dropBlock.lowered = True
		
		exprs = [
			tempSymbol, 
			tempAsgn, 
			valueRef.dropBlock, 
			tempRef
		]
		
		return block.Block(exprs, valueRef.span)
	
	def analyze(valueRef, state, implicitType):
		if valueRef.symbol == None:
			if not valueRef.dropBlock:
				valueRef.symbol = state.lookupSymbol(valueRef)
			if valueRef.symbol == None:
				return
		
		valueRef.symbol.unused = False
		valueRef.type = valueRef.symbol.type
		if valueRef.deref:
			valueRef.type = valueRef.type.baseType
		
		if not (valueRef.write or valueRef.fieldAccess or valueRef.addr):
			state.scope.readSymbol(valueRef)
		
		if implicitType and canPromote(valueRef.type, implicitType):
			coercion = Coercion(valueRef, None, valueRef.span)
			coercion.type = implicitType
			return coercion
		
		if valueRef.borrows and not valueRef.dropBlock:
			assert state.scope.dropBlock
			valueRef.dropBlock = state.scope.dropBlock
	
	def writeIR(ref, state):
		if type(ref.symbol) == staticdecl.ConstDecl:
			ref.symbol.expr.writeIR(state)
		elif type(ref.symbol) == staticdecl.StaticDecl:
			state.appendInstr(Global(ref, IPTR, ref.symbol.mangledName))
			state.appendInstr(Deref(ref, ref.type))
		elif type(ref.symbol) == fndecl.FnDecl:
			state.appendInstr(Global(ref, IPTR, ref.symbol.mangledName))
		elif type(ref.symbol) in (letdecl.LetDecl, letdecl.FnParam):
			stackOffset = state.localOffset(ref.symbol)
			if ref.copy:
				state.appendInstr(Dup(ref, stackOffset))
			elif stackOffset > 0:
				state.appendInstr(Raise(ref, stackOffset))
		else:
			assert 0
	
	def pretty(self, output, indent=0):
		closeParen = False
		if self.copy:
			output.addPrefix('$copy(')
			closeParen = True
		elif not (self.write or self.fieldAccess or self.addr) and \
			type(self.symbol) in (staticdecl.StaticDecl, letdecl.LetDecl, letdecl.FnParam):
			output.addPrefix('$move(')
			closeParen = True
		
		if self.path:
			path = '::'.join(tok.content for tok in self.path)
			output.write(path, indent)
		else:
			output.write(self.name, indent)
		
		if closeParen:
			output.write(')')
