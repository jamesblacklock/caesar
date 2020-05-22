from .ast   import ValueExpr, StaticDataType
from .log   import logError
from .types import canCoerce, typesMatch, OwnedType
from .ir    import FundamentalType, IExtend, Extend, Truncate, FExtend, FTruncate, IToF, UToF, FToI, FToU

class Coercion(ValueExpr):
	def __init__(self, expr, typeRef, span, resolvedType=None):
		super().__init__(span)
		self.expr = expr
		self.typeRef = typeRef
		self.type = resolvedType
	
	def analyze(asExpr, state, implicitType):
		asExpr.type = state.resolveTypeRef(asExpr.typeRef)
		asExpr.expr = state.analyzeNode(asExpr.expr, asExpr.type)
		
		if typesMatch(asExpr.expr.type, asExpr.type):
			return asExpr.expr
		
		if asExpr.expr.type.isOwnedType and not asExpr.type.isOwnedType:
			t = asExpr.expr.type
			asExpr.type = OwnedType(asExpr.type, t.acquire, t.release, t.acquireSpan, t.releaseSpan)
		
		if not canCoerce(asExpr.expr.type, asExpr.type):
			logError(state, asExpr.span, 'cannot coerce from {} to {}'
				.format(asExpr.expr.type, asExpr.type))
	
	def staticEval(self, state):
		staticValue = self.expr.staticEval(state)
		if staticValue == None:
			return None
		
		if staticValue.dataType == StaticDataType.INT and self.type.isIntLikeType:
			outOfRange = False
			if staticValue.data < self.type.MIN:
				outOfRange = True
				staticValue.data = self.type.MIN
			elif staticValue.data > self.type.MAX:
				outOfRange = True
				staticValue.data = self.type.MAX
			
			if outOfRange:
				logWarning(state, self.expr.span, 'value out of range for type `{}` and will be clamped'
					.format(self.type))
			
			staticValue.fType = FundamentalType.fromResolvedType(self.type)
			return staticValue
		else:
			return None
	
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
		if fromType.isFloatType and toType.isFloatType:
			if fromType.byteSize < toType.byteSize:
				instr = FExtend(expr, toType)
			else:
				instr = FTruncate(expr, toType)
		elif fromType.isFloatType:
			if toSigned:
				instr = FToI(expr, toType)
			else:
				instr = FToU(expr, toType)
		elif toType.isFloatType:
			if fromSigned:
				instr = IToF(expr, toType)
			else:
				instr = UToF(expr, toType)
		else:
			if fromType.byteSize < toType.byteSize:
				if toSigned:
					instr = IExtend(expr, toType)
				else:
					instr = Extend(expr, toType)
			else:
				instr = Truncate(expr, toType)
		
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