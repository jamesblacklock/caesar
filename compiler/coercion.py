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
		if exprIsInt and typeIsInt and asExpr.expr.type.byteSize == asExpr.type.byteSize:
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