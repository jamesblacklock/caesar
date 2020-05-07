from .ast   import ValueExpr
from .log   import logError
from .types import canCoerce, typesMatch
from .ir    import FundamentalType, IExtend, Extend, Truncate, FExtend, FTruncate, IToF, UToF, FToI, FToU

class Coercion(ValueExpr):
	def __init__(self, expr, typeRef, span):
		super().__init__(span)
		self.expr = expr
		self.typeRef = typeRef
	
	def analyze(asExpr, state, implicitType):
		asExpr.type = state.resolveTypeRef(asExpr.typeRef)
		asExpr.expr = state.analyzeNode(asExpr.expr, asExpr.type)
		
		if typesMatch(asExpr.expr.type, asExpr.type):
			return asExpr.expr
		
		if not canCoerce(asExpr.expr.type, asExpr.type):
			logError(state, asExpr.span, 'cannot coerce from {} to {}'
				.format(asExpr.expr.type, asExpr.type))
		
		exprIsInt = asExpr.expr.type and (asExpr.expr.type.isIntType or asExpr.expr.type.isPtrType)
		typeIsInt = asExpr.type and (asExpr.type.isIntType or asExpr.type.isPtrType)
		if exprIsInt and typeIsInt and asExpr.expr.type.byteSize == asExpr.type.byteSize and \
			asExpr.expr.type.isSigned == asExpr.type.isSigned:
			asExpr.expr.type = asExpr.type
			return asExpr.expr
	
	def writeIR(expr, state):
		expr.expr.writeIR(state)
		
		if expr.type.isVoidType:
			return
		
		fromType = FundamentalType.fromResolvedType(expr.expr.type)
		toType = FundamentalType.fromResolvedType(expr.type)
		
		fromSigned = expr.expr.type.isSigned
		toSigned = expr.type.isSigned
		
		if fromType.byteSize == toType.byteSize and fromType.isFloatType == toType.isFloatType:
			return
		
		instr = None
		if fromType.isFloatType == False and toType.isFloatType == False:
			if fromType.byteSize < toType.byteSize:
				if toSigned:
					instr = IExtend(expr, toType)
				else:
					instr = Extend(expr, toType)
			else:
				instr = Truncate(expr, toType)
		elif fromType.isFloatType == True and toType.isFloatType == True:
			if fromType.byteSize < toType.byteSize:
				instr = FExtend(expr, toType)
			else:
				instr = FTruncate(expr, toType)
		elif fromType.isFloatType == False:
			if fromSigned:
				instr = IToF(expr, toType)
			else:
				instr = UToF(expr, toType)
		else:
			if toSigned:
				instr = FToI(expr, toType)
			else:
				instr = FToU(expr, toType)
		
		state.appendInstr(instr)
		
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write(' as ')
		if self.typeRef:
			self.typeRef.pretty(output)
		else:
			output.write(self.type.name)

# class Coercion(ValueExpr):
		# if self.expr.isConstExpr:
		# 	self.isConstExpr = self.expr.isConstExpr
		# 	if self.type.isVoidType and self.type.isPrimitiveType:
		# 		self.bytes = []
		# 	elif self.expr.type.isIntType and self.type.isFloatType:
		# 		assert 0
		# 	elif self.expr.type.isFloatType and self.type.isIntType:
		# 		assert 0
		# 	elif (self.expr.type.isIntType or self.expr.type.isPtrType) and \
		# 		(self.type.isIntType or self.type.isPtrType):
		# 		if self.type.byteSize < self.expr.type.byteSize:
		# 			self.bytes = self.expr.bytes[:self.type.byteSize]
		# 		elif self.type.byteSize > self.expr.type.byteSize:
		# 			count = self.type.byteSize - self.expr.type.byteSize
		# 			b = 0xff if self.expr.value < 0 else 0x00
		# 			self.bytes = list(self.expr.bytes)
		# 			for _ in range(0, count): self.bytes.append(b)
		# 		else:
		# 			self.bytes = self.expr.bytes
		# 	elif self.expr.type.isFloatType and self.type.isFloatType:
		# 		assert 0
		# 	else:
		# 		assert 0