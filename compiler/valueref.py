from .ast      import ValueExpr, StaticDecl, FnParam
from .         import fndecl
from .         import letdecl
from .         import constdecl
from .coercion import Coercion
from .types    import canPromote
from .ir       import IPTR, Dup, Raise, Global, Deref

class ValueRef(ValueExpr):
	def __init__(self, path, span, name=None, temp=False):
		super().__init__(span)
		self.path = path
		self.symbol = None
		self.copy = False
		self.write = False
		self.fieldAccess = False
		self.nameTok = path[-1] if path else None
		self.temp = temp
		if path:
			self.name = self.nameTok.content
		else:
			self.name = '???'
	
	def analyze(valueRef, state, implicitType):
		if valueRef.symbol == None:
			valueRef.symbol = state.lookupSymbol(valueRef)
		if valueRef.symbol:
			if valueRef.temp:
				valueRef.name = valueRef.symbol.name
			valueRef.symbol.unused = False
			valueRef.type = valueRef.symbol.type
			if not (valueRef.write or valueRef.fieldAccess):
				state.scope.readSymbol(valueRef)
		
		if implicitType and canPromote(valueRef.type, implicitType):
			coercion = Coercion(valueRef, None, valueRef.span)
			coercion.type = implicitType
			return coercion
	
	def writeIR(ref, state):
		if type(ref.symbol) == constdecl.ConstDecl:
			ref.symbol.expr.writeIR(state)
		elif type(ref.symbol) == StaticDecl:
			state.appendInstr(Global(ref, IPTR, ref.symbol.mangledName))
			state.appendInstr(Deref(ref, ref.type))
		elif type(ref.symbol) == fndecl.FnDecl:
			state.appendInstr(Global(ref, IPTR, ref.symbol.mangledName))
		elif type(ref.symbol) in (letdecl.LetDecl, FnParam):
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
		elif not self.write and not self.fieldAccess and type(self.symbol) in (StaticDecl, letdecl.LetDecl, FnParam):
			output.addPrefix('$move(')
			closeParen = True
		
		if self.path:
			path = '::'.join(tok.content for tok in self.path)
			output.write(path, indent)
		else:
			output.write(self.name, indent)
		
		if closeParen:
			output.write(')')
