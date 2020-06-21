from .ast    import AST, ValueSymbol
from .       import access, block
from .types  import typesMatch, PtrType
from .log    import logError
from .ir     import Res, Fix, FundamentalType

TEMP_COUNTER = 0

class CVarArgsParam(AST):
	def __init__(self, span):
		super().__init__(span)

class FnParam(ValueSymbol):
	def __init__(self, nameTok, typeRef, span):
		super().__init__(nameTok, typeRef, span)
		self.dropFn = None
		self.mut = False
		# self.defaultExpr = defaultExpr
		self.fixed = False
		self.dropBlock = None
	
	def analyze(param, state, implicitType):
		param.dropBlock = state.scope.fnDecl.body
		state.scope.declSymbol(param)
		LetDecl.checkDropFn(param, state)
	
	def pretty(self, output, indent=0):
		output.write(self.name, indent)
		output.write(': ')
		self.typeRef.pretty(output)

class LetDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, mut, expr, span, temp=False):
		super().__init__(nameTok, typeRef, span)
		if self.name == '_':
			temp = True
		if temp:
			global TEMP_COUNTER
			self.name = '$temp{}'.format(TEMP_COUNTER)
			TEMP_COUNTER += 1
		self.dropFn = None
		self.mut = mut
		self.expr = expr
		self.temp = temp
		self.fixed = False
		self.reserve = False
		self.dropBlock = None
	
	@staticmethod
	def createTemp(span):
		return LetDecl(None, None, False, None, span, temp=True)
	
	def checkDropFn(letExpr, state):
		if letExpr.dropFn == None:
			letExpr.dropFn = letExpr.type.dropFn
		elif letExpr.type.dropFn:
			logError(state, letExpr.nameTok.span, 
				'cannot use @drop on a type that already has a drop function')
			letExpr.dropFn = letExpr.type.dropFn
		
		if letExpr.dropFn == None:
			return
		
		if len(letExpr.dropFn.params) != 1 or letExpr.dropFn.cVarArgs:
			logError(state, letExpr.nameTok.span, 'drop function must take 1 argument')
			letExpr.dropFn = None
			return
		
		t = PtrType(letExpr.dropFn.params[0].type, 1, False)
		if not typesMatch(t, letExpr.type):
			logError(state, letExpr.nameTok.span, 
				'drop function receives the wrong type of argument (expected {}, found {})'.format(
					t, letExpr.type))
			letExpr.dropFn = None
			return
	
	def analyze(letExpr, state, implicitType):
		if letExpr.typeRef:
			letExpr.type = state.resolveTypeRef(letExpr.typeRef)
		
		state.scope.declSymbol(letExpr)
		
		result = letExpr
		if letExpr.expr:
			result = access.SymbolAccess.analyzeSymbolAccess(state, letExpr, letExpr.type)
		
		if letExpr.type:
			letExpr.checkDropFn(state)
		
		return result
	
	def accessSymbols(self, scope):
		pass
	
	def writeIR(ast, state):
		if ast.reserve:
			assert ast.type
			fType = FundamentalType.fromResolvedType(ast.type)
			state.appendInstr(Res(ast, fType))
			state.nameTopOperand(ast)
			if ast.fixed:
				state.appendInstr(Fix(ast, 0))
	
	def pretty(self, output, indent=0):
		output.write('let ', indent)
		if self.mut:
			output.write('mut ')
		output.write(self.name)
		if self.expr:
			output.write(' = ')
			self.expr.pretty(output)