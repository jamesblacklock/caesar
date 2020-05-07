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
		self.write = False
		self.lowered = False
		self.dropBlock = None
	
	def lower(indexOp, state):
		if indexOp.lowered:
			return indexOp
		else:
			indexOp.lowered = True
		
		exprs = []
		
		# if type(indexOp.expr) == valueref.ValueRef:
		# 	temp = letdecl.LetDecl(None, None, False, None, indexOp.span, temp=True)
			
		# 	tempLValue = valueref.ValueRef(None, indexOp.span, temp=True)
		# 	tempLValue.symbol = temp
		# 	tempAsgn = asgn.Asgn(tempLValue, indexOp, indexOp.span, temp=True)
		# 	tempAsgn.lowered = True
			
		# 	indexOp.dropBlock = block.Block([], None)
		# 	indexOp.dropBlock.lowered = True
			
		# 	tempRef = valueref.ValueRef(None, indexOp.span, temp=True)
		# 	tempRef.symbol = temp
			
		# 	exprs = [temp, tempAsgn, indexOp.dropBlock, tempRef]
		# 	return block.Block(exprs, indexOp.span, scope.ScopeType.BLOCK)
		# else:
		
		if type(indexOp.expr) == valueref.ValueRef:
			pass
		elif type(indexOp.expr) == deref.Deref:
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
				(tempSymbol, tempAsgn, tempRef) = letdecl.createTempTriple(tempRValue)
				exprs.append(tempSymbol)
				exprs.append(tempAsgn)
				indexOp.expr = tempRef
		else:
			assert 0
		
		assert type(indexOp.expr) == valueref.ValueRef
		
		if indexOp.write:
			exprs.append(indexOp)
		else:
			indexOp.dropBlock = block.Block([], indexOp.span)
			indexOp.dropBlock.lowered = True
			(tempSymbol, tempAsgn, tempRef) = letdecl.createTempTriple(indexOp)
			exprs.extend([tempSymbol, tempAsgn, indexOp.dropBlock, tempRef])
		
		b = block.Block(exprs, indexOp.span, scope.ScopeType.BLOCK)
		b.lowered = True
		return b

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
		
		if not expr.write:
			state.scope.readSymbol(expr)
	
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
		baseType = ast.expr.type.baseType.baseType if ast.deref else ast.expr.type.baseType
		mul = types.getAlignedSize(baseType)
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
			expr.dropBlock = block.Block([], expr.span)
			expr.dropBlock.lowered = True
			(tempSymbol, tempAsgn, tempRef) = letdecl.createTempTriple(expr)
			exprs = [tempSymbol, tempAsgn, expr.dropBlock, tempRef]
			return block.Block(exprs, expr.span, scope.ScopeType.BLOCK)
		else:
			if type(expr.expr) in (deref.Deref, Index):
				assert 0
			
			(temp1, temp1Asgn, temp1Ref) = letdecl.createTempTriple(expr.expr)
			expr.expr = temp1Ref
			(temp2, temp2Asgn, temp2Ref) = letdecl.createTempTriple(expr)
			
			exprs = [temp1, temp1Asgn, temp2, temp2Asgn, temp1Asgn.dropBlock, temp2Ref]
			return block.Block(exprs, expr.span, scope.ScopeType.BLOCK)
	
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