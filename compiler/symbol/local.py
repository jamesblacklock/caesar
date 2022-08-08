from .symbol   import ValueSymbol
from ..mir.mir import MIR, TypeModifiers
from ..ir      import FundamentalType, Res
from ..types   import PtrType, typesMatch
from ..log     import logError

TEMP_COUNTER = 0

class Local(ValueSymbol):
	def __init__(self, ast, type, isParam, tempSpan=None):
		if tempSpan:
			self.temp = True
			self.mut = False
			self.attrDropFn = None
			
			name = None
			nameSpan = tempSpan
			span = tempSpan
		else:
			self.temp = False
			self.mut = ast.mut
			self.attrDropFn = ast.dropFn
			
			name = None if ast.name == '_' else ast.name
			nameSpan = ast.nameSpan
			span = ast.span
		
		if name == None:
			global TEMP_COUNTER
			name = '${}'.format(TEMP_COUNTER)
			TEMP_COUNTER += 1
		
		super().__init__(name, nameSpan, span, False, type)
		
		self.ast = ast
		self.unused = True
		self.isLocal = True
		self.isParam = isParam
		self.dropPoint = None
		self.dropFn = None
		self.fixed = False
		self.reserve = False
		self.typeModifiers = TypeModifiers()
		self.contracts = {}
	
	@staticmethod
	def createTemp(span):
		return Local(None, None, False, span)
	
	def resolveGenericParam(self, genericInc):
		assert self.isParam
		return Local(self.ast, self.type.resolveGenerics(genericInc), True)
	
	def checkSig(self, state):
		assert self.isParam
		self.type = self.ast.typeRef.resolveSig(state)
		if self.type or self.dropFn:
			self.checkDropFn(state)
	
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
	
	# def writeIR(self, state):
	# 	if self.reserve:
	# 		assert self.type
	# 		fType = FundamentalType.fromResolvedType(self.type)
	# 		state.appendInstr(Res(self, fType))
	# 		state.nameTopOperand(self)
	# 		if self.fixed:
	# 			state.appendInstr(Fix(self, 0))
	
	def __str__(self):
		type = ': {}'.format(self.type.name) if self.type else ''
		return '$local {}{}{}'.format('mut ' if self.mut else '', self.name, type)