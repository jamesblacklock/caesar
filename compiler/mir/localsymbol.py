from .mir    import MIR, TypeModifiers
from ..ir    import FundamentalType, Res
from ..types import PtrType, typesMatch
from ..log   import logError

TEMP_COUNTER = 0

class LocalSymbol(MIR):
	def __init__(self, name, type, mut, isParam, attrDropFn, span):
		super().__init__(span)
		if name == None:
			self.temp = True
			global TEMP_COUNTER
			name = '$temp{}'.format(TEMP_COUNTER)
			TEMP_COUNTER += 1
		self.name = name
		self.unused = True
		self.isParam = isParam
		self.type = type
		self.mut = mut
		self.attrDropFn = attrDropFn
		self.dropFn = None
		self.fixed = False
		self.reserve = False
		self.typeModifiers = TypeModifiers()
	
	@staticmethod
	def createTemp(span):
		return LocalSymbol(None, None, False, False, None, span)
	
	def analyze(self, state, implicitType):
		if self.temp:
			state.scope.declSymbol(self)
		return self
	
	def checkDropFn(self, state):
		if self.type and self.type.dropFn:
			if self.attrDropFn:
				logError(state, self.span, 'cannot use @drop on a type that already has a drop function')
			self.dropFn = self.type.dropFn
		elif self.dropFn: # this happens when the type of the symbol gets changed (search canChange) and dropFn is re-checked
			return
		else:
			self.dropFn = self.attrDropFn
		
		if self.dropFn == None:
			return
		
		if len(self.dropFn.params) != 1 or self.dropFn.cVarArgs:
			logError(state, self.span, 'drop function must take 1 argument')
			self.dropFn = None
			return
		
		if not self.type:
			return
		
		t = PtrType(self.type, 1, True)
		if not typesMatch(t, self.dropFn.params[0].type):
			logError(state, self.span, 'drop function receives the wrong type of argument (expected {}, found {})'.format(
					t, self.dropFn.params[0].type))
			self.dropFn = None
			return
	
	def checkFlow(self, scope):
		pass
	
	def writeIR(self, state):
		if self.reserve:
			assert self.type
			fType = FundamentalType.fromResolvedType(self.type)
			state.appendInstr(Res(self, fType))
			state.nameTopOperand(self)
			if self.fixed:
				state.appendInstr(Fix(self, 0))
	
	def __str__(self):
		type = ': {}'.format(self.type.name) if self.type else ''
		return '$local {}{}{}'.format('mut ' if self.mut else '', self.name, type)