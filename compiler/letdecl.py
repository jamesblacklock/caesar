from .ast    import ValueSymbol
from .       import valueref
from .       import asgn as asgnmod
from .       import block
from .types  import getValidAssignType, typesMatch, PtrType
from .log    import logError

TEMP_COUNTER = 0

class LetDecl(ValueSymbol):
	def __init__(self, nameTok, typeRef, mut, expr, span, temp=False):
		super().__init__(nameTok, typeRef, span)
		if temp:
			global TEMP_COUNTER
			self.name = '$temp{}'.format(TEMP_COUNTER)
			TEMP_COUNTER += 1
		self.dropFn = None
		self.mut = mut
		self.expr = expr
		self.noBinding = False
		self.block = None
		self.temp = temp
		self.fixed = False
	
	def checkDropFn(letExpr, state):
		if letExpr.dropFn == None:
			return
		
		if letExpr.type.dropFn:
			logError(state, letExpr.nameTok.span, 
				'cannot use @drop on a type that already has a drop function')
			letExpr.dropFn = None
			return
		
		if len(letExpr.dropFn.params) > 1 or letExpr.dropFn.cVarArgs:
			logError(state, letExpr.nameTok.span, 'drop function must take exactly 0 or 1 arguments')
			letExpr.dropFn = None
			return
		
		if len(letExpr.dropFn.params) == 1:
			t = letExpr.dropFn.params[0].type
			if not typesMatch(t, letExpr.type) and \
				not typesMatch(t, PtrType(letExpr.type, 1)):
				logError(state, letExpr.nameTok.span, 
					'drop function receives the wrong type of argument (expected {}, found {})'.format(
						t, letExpr.type))
				letExpr.dropFn = None
				return
	
	def analyze(letExpr, state, implicitType):
		if letExpr.typeRef:
			letExpr.type = state.resolveTypeRef(letExpr.typeRef)
		
		if letExpr.name == '_':
			letExpr.noBinding = True
		else:
			state.scope.declSymbol(letExpr)
		
		letExpr.block = block.Block(block.BlockInfo([letExpr], letExpr.span))
		
		if letExpr.expr:
			rvalue = letExpr.expr
			letExpr.expr = None
			
			lvalue = valueref.ValueRef([letExpr.nameTok], letExpr.nameTok.span)
			asgn = asgnmod.Asgn(lvalue, rvalue, letExpr.span)
			asgn.lowered = True
			asgn = state.analyzeNode(asgn)
			# asgn.rvalue = state.analyzeNode(asgn.rvalue, letExpr.type)
			# if letExpr.type == None:
			# 	letExpr.type = rvalue.type
			
			# asgn.lvalue = state.analyzeNode(asgn.lvalue)
			
			# if getValidAssignType(asgn.lvalue.type, asgn.rvalue.type) == None:
			# 	logError(state, asgn.rvalue.span, 
			# 		'expected type {}, found {}'.format(asgn.symbol.type, asgn.rvalue.type))
			
			letExpr.block.exprs.append(asgn)
			
			if state.scope.dropBlock == None:
				asgn.dropBlock = block.Block(block.BlockInfo([], None))
				asgn.dropBlock.lowered = True
				letExpr.block.exprs.append(asgn.dropBlock)
				state.scope.dropBlock = asgn.dropBlock
		
		if letExpr.type:
			letExpr.checkDropFn(state)
		
		return letExpr.block
	
	def writeIR(ast, state):
		pass
	
	def pretty(self, output, indent=0):
		output.write('let ', indent)
		if self.mut:
			output.write('mut ')
		output.write(self.name)
		if self.expr:
			output.write(' = ')
			self.expr.pretty(output)