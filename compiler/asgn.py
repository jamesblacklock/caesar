from .ast       import AST
from .          import valueref, letdecl, block, deref
from .structlit import StructLit
from .field     import Index, Field
from .ifexpr    import If
from .types     import typesMatch
from .ir        import Swap, DerefW, FieldW, DerefFieldW, IPTR
from .scope     import ScopeType
from .log       import logError

class Asgn(AST):
	def __init__(self, lvalue, rvalue, span, temp=False):
		super().__init__(span)
		lvalue.write = True
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.block = None
		self.dropBlock = None
		self.dropBeforeAssignBlock = None
		self.lowered = False
		self.temp = temp
		self.hasRValueTempSymbol = False
	
	def lowerLValue(asgn, state):
		asgn.dropBlock = block.Block(block.BlockInfo([], asgn.span))
		asgn.dropBlock.lowered = True
		
		if type(asgn.lvalue) == valueref.ValueRef:
			asgn.block = block.Block(block.BlockInfo([asgn], asgn.span))
			asgn.block.lowered = True
		elif type(asgn.lvalue) in (deref.Deref, Index):
			asgn.block = asgn.lvalue.lower(state)
			asgn.lvalue = asgn.block.exprs[-1]
			asgn.block.exprs[-1] = asgn
		else:
			assert 0
		
		asgn.block.exprs.append(asgn.dropBlock)
		return
		
		
		
		
		
		
		tempRValue = None
		if type(asgn.lvalue) == deref.Deref:
			if asgn.lvalue.count > 1:
				asgn.lvalue.count -= 1
				tempRValue = asgn.lvalue
			elif type(asgn.lvalue.expr) != valueref.ValueRef:
				tempRValue = asgn.lvalue.expr
		elif type(asgn.lvalue) in (Field, Index):
			baseParent = asgn.lvalue
			while type(baseParent.expr) in (Field, Index):
				baseParent = baseParent.expr
			if type(baseParent.expr) not in (valueref.ValueRef, deref.Deref):
				tempRValue = baseParent.expr
			elif type(baseParent.expr) == deref.Deref:
				if baseParent.expr.count > 1:
					baseParent.expr.count -= 1
					tempRValue = baseParent.expr
				elif type(baseParent.expr.expr) != valueref.ValueRef:
					tempRValue = baseParent.expr.expr
		else:
			assert 0
		
		if tempRValue:
			assert 0
	
	def lowerRValue(asgn, state):
		tempSymbol = letdecl.LetDecl(None, None, False, None, None, temp=True)
		
		tempLValue = valueref.ValueRef(None, None, temp=True)
		tempLValue.symbol = tempSymbol
		tempAsgn = Asgn(tempLValue, asgn.rvalue, None, temp=True)
		tempAsgn.dropBlock = block.Block(block.BlockInfo([], None))
		tempAsgn.lowered = True
		
		tempRef = valueref.ValueRef(None, None, temp=True)
		tempRef.symbol = tempSymbol
		
		asgn.dropBeforeAssignBlock = block.Block(block.BlockInfo([], asgn.span))
		asgn.dropBeforeAssignBlock.lowered = True
		
		exprs = [
			tempSymbol, 
			tempAsgn, 
			asgn.dropBeforeAssignBlock, 
			tempRef
		]
		
		asgn.rvalue = block.Block(block.BlockInfo(exprs, asgn.rvalue.span), ScopeType.BLOCK)
		asgn.rvalue.lowered = True
	
	def lower(asgn, state):
		if asgn.lowered:
			return asgn
		else:
			asgn.lowered = True
			asgn.hasRValueTempSymbol = True
		
		asgn.lowerRValue(state)
		asgn.lowerLValue(state)
		
		return asgn.block
	
	def analyze(asgn, state, ignoredImplicitType):
		asgn.lvalue = state.analyzeNode(asgn.lvalue)
		if asgn.hasRValueTempSymbol:
			asgn.rvalue.exprs[0].type = asgn.lvalue.type
		asgn.rvalue = state.analyzeNode(asgn.rvalue, asgn.lvalue.type)
		
		if asgn.lvalue.deref:
			state.scope.readSymbol(asgn.lvalue)
		elif type(asgn.lvalue) == Field and type(asgn.lvalue.expr) == valueref.ValueRef:
			assert 0
			state.scope.writeSymbol(asgn, asgn.lvalue.field)
		elif type(asgn.lvalue) in (Field, deref.Deref):
			assert 0
		elif type(asgn.lvalue) == Index:
			state.scope.writeSymbol(asgn)
		elif type(asgn.lvalue) == valueref.ValueRef:
			state.scope.writeSymbol(asgn)
			if asgn.lvalue.symbol.type == None:
				asgn.lvalue.type = asgn.rvalue.type
				asgn.lvalue.symbol.type = asgn.rvalue.type
				asgn.lvalue.symbol.checkDropFn(state)
		else:
			assert 0
		
		if not typesMatch(asgn.lvalue.type, asgn.rvalue.type):
			logError(state, asgn.rvalue.span, 
				'expected type {}, found {}'.format(asgn.lvalue.type, asgn.rvalue.type))
		
		return asgn
	
	def writeIR(expr, state):
		if expr.lvalue.type.isVoidType:
			expr.rvalue.writeIR(state)
			if not expr.rvalue.type.isVoidType:
				state.appendInstr(Pop(expr))
			return
		
		if expr.lvalue.deref:
			expr.lvalue.writeIR(state)
			expr.rvalue.writeIR(state)
			state.appendInstr(DerefW(expr))
		elif type(expr.lvalue) == valueref.ValueRef:
			expr.rvalue.writeIR(state)
			if expr.lvalue.symbol in state.operandsBySymbol:
				stackOffset = state.localOffset(expr.lvalue.symbol)
				if stackOffset > 0:
					state.appendInstr(Swap(expr, stackOffset))
			else:
				state.nameTopOperand(expr.lvalue.symbol)
			
			if state.loopInfo:
				state.loopInfo.droppedSymbols.discard(expr.lvalue.symbol)
		elif type(expr.lvalue) == Index:
			assert type(expr.lvalue.expr) == valueref.ValueRef
			expr.rvalue.writeIR(state)
			expr.lvalue.index.writeIR(state)
			stackOffset = state.localOffset(expr.lvalue.expr.symbol)
			if expr.lvalue.deref:
				state.appendInstr(DerefFieldW(expr, stackOffset))
			else:
				state.appendInstr(FieldW(expr, stackOffset))
		else:
			assert 0
		
		# if type(expr) != ValueRef:
		# 	swap = True
		# 	ast.lvalue.writeIR(state)
		
		# ast.rvalue.writeIR(state)
		
		# if type(ast.lvalue) == FieldAccess:
		# 	state.appendInstr(Imm(ast, IPTR, ast.lvalue.fieldOffset))
		# else:
		# 	ast.lvalue.index.writeIR(state)
		
		
		# else:
		# 	stackOffset = 2
		
		
		
		
		# if type(ast.lvalue) == Deref:
		# 	ast.lvalue.writeIR(state)
		# 	for _ in range(0, ast.lvalue.derefCount-1):
		# 		state.appendInstr(Deref(ast.lvalue, IPTR))
		# 	ast.rvalue.writeIR(state)
		# 	state.appendInstr(DerefW(ast))
		# elif type(ast.lvalue) in (FieldAccess, IndexOp):
		# 	swap = False
		# 	deref = False
		# 	expr = ast.lvalue
		# 	if type(expr) == Deref:
		# 		deref = True
		# 		expr = expr.expr
			
		# 	if type(expr) != ValueRef:
		# 		swap = True
		# 		ast.lvalue.writeIR(state)
			
		# 	ast.rvalue.writeIR(state)
			
		# 	if type(ast.lvalue) == FieldAccess:
		# 		state.appendInstr(Imm(ast, IPTR, ast.lvalue.fieldOffset))
		# 	else:
		# 		ast.lvalue.index.writeIR(state)
			
		# 	if type(expr) == ValueRef:
		# 		stackOffset = state.localOffset(expr.symbol)
		# 	else:
		# 		stackOffset = 2
			
		# 	if deref:
		# 		state.appendInstr(DerefFieldW(ast, stackOffset))
		# 	else:
		# 		state.appendInstr(FieldW(ast, stackOffset))
			
		# 	if swap:
		# 		state.appendInstr(Swap(ast, stackOffset))
	
	def pretty(self, output, indent=0):
		self.lvalue.pretty(output, indent)
		if self.lvalue.deref:
			output.write('^')
		output.write(' = ')
		if type(self.rvalue) not in (block.Block, If, StructLit):
			indent = 0
		elif type(self.rvalue) in (If, StructLit):
			output.write('\n')
			indent += 1
		
		self.rvalue.pretty(output, indent)
