from .ast   import ValueExpr
from .types import USize
from .log   import logError
from .token import TokenType
from .      import deref, valueref, letdecl, asgn, block, types, ir, scope

class Index(ValueExpr):
	def __init__(self, expr, index, span):
		super().__init__(span)
		self.expr = expr
		self.index = index
		self.deref = False
	
	def lower(indexOp, state):
		if type(indexOp.expr) == valueref.ValueRef:
			return indexOp
		
		if type(indexOp.expr) == deref.Deref:
			indexOp.deref = True
			tempRValue = None
			if indexOp.expr.count > 1:
				indexOp.deref = True
				indexOp.expr.count -= 1
				tempRValue = indexOp.expr
			elif type(indexOp.expr.expr) != valueref.ValueRef:
				tempRValue = indexOp.expr.expr
			else:
				indexOp.expr = indexOp.expr.expr
			
			if tempRValue:
				temp1 = letdecl.LetDecl(None, None, False, None, indexOp.span, temp=True)
				
				temp1LValue = valueref.ValueRef(None, indexOp.span, temp=True)
				temp1LValue.symbol = temp1
				temp1Asgn = asgn.Asgn(temp1LValue, tempRValue, indexOp.span, temp=True)
				temp1Asgn.lowered = True
				temp1Asgn.dropBlock = block.Block(block.BlockInfo([], None))
				temp1Asgn.dropBlock.lowered = True
				
				temp2 = letdecl.LetDecl(None, None, False, None, indexOp.span, temp=True)
				
				indexOp.expr = valueref.ValueRef(None, indexOp.span, temp=True)
				indexOp.expr.symbol = temp1
				temp2LValue = valueref.ValueRef(None, indexOp.span, temp=True)
				temp2LValue.symbol = temp2
				temp2Asgn = asgn.Asgn(temp2LValue, indexOp, indexOp.span, temp=True)
				temp2Asgn.lowered = True
				
				temp2Ref = valueref.ValueRef(None, indexOp.span, temp=True)
				temp2Ref.symbol = temp2
				
				exprs = [temp1, temp1Asgn, temp2, temp2Asgn, temp1Asgn.dropBlock, temp2Ref]
				return block.Block(block.BlockInfo(exprs, indexOp.span))
			else:
				return indexOp
		
		if type(indexOp.expr) in (deref.Deref, Index):
			assert 0
		else:
			assert 0

	def analyze(expr, state, implicitType):
		assert type(expr.expr) == valueref.ValueRef
		expr.expr.fieldAccess = True
		
		# expr.expr.fieldAccess = True
		expr.expr = state.analyzeNode(expr.expr)
		expr.index = state.analyzeNode(expr.index, USize)
		
		if expr.expr.type:
			if expr.expr.type.isArrayType:
				expr.type = expr.expr.type.baseType
			elif expr.deref and expr.expr.type.baseType.isArrayType:
				expr.type = expr.expr.type.baseType.baseType
			else:
				logError(state, expr.expr.span, 
					'base of index expression must be an array type (found {})'.format(expr.expr.type))
		
		if expr.index.type and expr.index.type != USize:
			logError(state, expr.index.span, 'index must be type usize (found {})'.format(expr.index.type))
	
	def writeIR(ast, state):
		# swap = False
		# if type(ast.expr) == Deref:
		# 	deref = True
		# 	exprToIR(state, ast.expr.expr)
		# 	for _ in range(0, ast.expr.derefCount-1):
		# 		state.appendInstr(ir.Deref(ast.expr, ir.IPTR))
		# 	stackOffset = 1
		# 	swap = True
		# elif type(ast.expr) == ValueRef:
		# else:
		# 	exprToIR(state, ast.expr)
		# 	stackOffset = 1
		# 	swap = True
		
		ast.index.writeIR(state)
		mul = types.getAlignedSize(ast.expr.type.baseType)
		if mul > 1:
			state.appendInstr(ir.Imm(ast, ir.IPTR, mul))
			state.appendInstr(ir.Mul(ast))
		
		stackOffset = state.localOffset(ast.expr.symbol)
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		if ast.deref:
			state.appendInstr(ir.DerefField(ast, stackOffset, fType))
		else:
			state.appendInstr(ir.Field(ast, stackOffset, fType))
		
		# if swap:
		# 	state.appendInstr(ir.Swap(ast, stackOffset))
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		if self.deref:
			output.write('^')
		output.write('[')
		self.index.pretty(output)
		output.write(']')

class Field(ValueExpr):
	def __init__(self, expr, path, span):
		super().__init__(span)
		self.expr = expr
		self.path = path
		self.deref = False
		self.write = False
		self.lowered = False
		self.dropBlock = None
	
	def lower(expr, state):
		if expr.lowered:
			return expr
		else:
			expr.lowered = True
		
		if type(expr.expr) == valueref.ValueRef:
			temp = letdecl.LetDecl(None, None, False, None, expr.span, temp=True)
			
			tempLValue = valueref.ValueRef(None, expr.span, temp=True)
			tempLValue.symbol = temp
			tempAsgn = asgn.Asgn(tempLValue, expr, expr.span, temp=True)
			tempAsgn.lowered = True
			
			expr.dropBlock = block.Block(block.BlockInfo([], None))
			expr.dropBlock.lowered = True
			
			tempRef = valueref.ValueRef(None, expr.span, temp=True)
			tempRef.symbol = temp
			
			exprs = [temp, tempAsgn, expr.dropBlock, tempRef]
			return block.Block(block.BlockInfo(exprs, expr.span), scope.ScopeType.BLOCK)
		else:
			if type(expr.expr) in (deref.Deref, Index):
				assert 0
			
			temp1 = letdecl.LetDecl(None, None, False, None, expr.span, temp=True)
			
			temp1LValue = valueref.ValueRef(None, expr.span, temp=True)
			temp1LValue.symbol = temp1
			temp1Asgn = asgn.Asgn(temp1LValue, expr.expr, expr.span, temp=True)
			temp1Asgn.lowered = True
			temp1Asgn.dropBlock = block.Block(block.BlockInfo([], None))
			temp1Asgn.dropBlock.lowered = True
			
			temp2 = letdecl.LetDecl(None, None, False, None, expr.span, temp=True)
			
			expr.expr = valueref.ValueRef(None, expr.span, temp=True)
			expr.expr.symbol = temp1
			temp2LValue = valueref.ValueRef(None, expr.span, temp=True)
			temp2LValue.symbol = temp2
			temp2Asgn = asgn.Asgn(temp2LValue, expr, expr.span, temp=True)
			temp2Asgn.lowered = True
			
			temp2Ref = valueref.ValueRef(None, expr.span, temp=True)
			temp2Ref.symbol = temp2
			
			exprs = [temp1, temp1Asgn, temp2, temp2Asgn, temp1Asgn.dropBlock, temp2Ref]
			return block.Block(block.BlockInfo(exprs, expr.span), scope.ScopeType.BLOCK)
	
	def analyze(expr, state, implicitType):
		assert type(expr.expr) == valueref.ValueRef
		expr.expr.fieldAccess = True
		
		expr.expr = state.analyzeNode(expr.expr)
		if expr.expr.type == None:
			return
		
		offset = 0
		field = None
		# errSpan = op.expr.span
		t = expr.expr.type
		for tok in expr.path:
			if t.isStructType:
				if tok.content not in t.fieldDict:
					logError(state, tok.span, 'type `{}` has no field `{}`'.format(t.name, tok.content))
					return
				
				field = t.fieldDict[tok.content]
			elif t.isTupleType:
				fieldIndex = None if tok.type != TokenType.INTEGER else int(tok.content)
				if fieldIndex == None or fieldIndex not in range(0, len(t.fields)):
					logError(state, tok.span, 'type `{}` has no field `{}`'.format(t.name, tok.content))
					return
				
				field = t.fields[fieldIndex]
			else:
				logError(state, op.span, 'type `{}` has no fields'.format(t.name))
				return
			
			offset += field.offset
			t = field.type
			if t == None:
				return
		
		expr.field = field
		expr.offset = offset
		expr.type = t
		
		if not expr.write:
			state.scope.readSymbol(expr)

	def writeIR(fieldOp, state):
		state.appendInstr(ir.Imm(fieldOp, ir.IPTR, fieldOp.offset))
		stackOffset = state.localOffset(fieldOp.expr.symbol)
		fType = ir.FundamentalType.fromResolvedType(fieldOp.type)
		if fieldOp.deref:
			state.appendInstr(ir.DerefField(fieldOp, stackOffset, fType))
		else:
			state.appendInstr(ir.Field(fieldOp, stackOffset, fType))
	
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		if self.deref:
			output.write('^')
		for p in self.path:
			output.write('.')
			output.write(p.content)