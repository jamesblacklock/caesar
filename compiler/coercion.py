from .ast   import ValueExpr, StaticDataType
from .log   import logError
from .types import canCoerce, typesMatch, OwnedType
from .ir    import FundamentalType
from .      import ir

class Coercion(ValueExpr):
	def __init__(self, expr, typeRef, span, resolvedType=None):
		super().__init__(span)
		self.expr = expr
		self.typeRef = typeRef
		self.type = resolvedType
	
	def analyze(asExpr, state, implicitType):
		asExpr.type = state.resolveTypeRef(asExpr.typeRef)
		asExpr.expr = state.analyzeNode(asExpr.expr, asExpr.type)
		
		if not asExpr.expr.type:
			return None
		elif typesMatch(asExpr.expr.type, asExpr.type):
			return asExpr.expr
		elif asExpr.expr.type.isOwnedType and not asExpr.type.isOwnedType:
			t = asExpr.expr.type
			asExpr.type = OwnedType(asExpr.type, t.acquire, t.release, t.acquireSpan, t.releaseSpan)
		
		if not canCoerce(asExpr.expr.type, asExpr.type):
			logError(state, asExpr.span, 'cannot coerce from {} to {}'
				.format(asExpr.expr.type, asExpr.type))
	
	def accessSymbols(self, scope):
		self.expr.accessSymbols(scope)
	
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
	
	def writeTraitCoercionIR(expr, state):
		vtblName = expr.expr.type.baseType.traitImpls[expr.type.baseType].vtblName
		fType = FundamentalType.fromResolvedType(expr.type)
		
		state.appendInstr(ir.Res(expr, fType))
		
		expr.expr.writeIR(state)
		state.appendInstr(ir.Imm(expr, ir.IPTR, 0))
		state.appendInstr(ir.FieldW(expr, 2))
		
		state.appendInstr(ir.Global(expr, ir.IPTR, vtblName))
		state.appendInstr(ir.Imm(expr, ir.IPTR, ir.IPTR.byteSize))
		state.appendInstr(ir.FieldW(expr, 2))
	
	def writeIR(expr, state):
		fromType = expr.expr.type
		toType = expr.type
		
		if fromType.isPtrType and not fromType.isTraitPtr and \
			toType.isPtrType and toType.isTraitPtr:
			expr.writeTraitCoercionIR(state)
			return
		
		expr.expr.writeIR(state)
		
		if expr.type.isVoidType:
			return
		
		fromSigned = expr.expr.type.isSigned
		toSigned = expr.type.isSigned
		fromType = FundamentalType.fromResolvedType(fromType)
		toType = FundamentalType.fromResolvedType(toType)
		
		if fromType.byteSize == toType.byteSize and fromType.isFloatType == toType.isFloatType:
			return
		
		instr = None
		if fromType.isFloatType and toType.isFloatType:
			if fromType.byteSize < toType.byteSize:
				instr = ir.FExtend(expr, toType)
			else:
				instr = ir.FTruncate(expr, toType)
		elif fromType.isFloatType:
			if toSigned:
				instr = ir.FToI(expr, toType)
			else:
				instr = ir.FToU(expr, toType)
		elif toType.isFloatType:
			if fromSigned:
				instr = ir.IToF(expr, toType)
			else:
				instr = ir.UToF(expr, toType)
		else:
			if fromType.byteSize < toType.byteSize:
				if toSigned:
					instr = ir.IExtend(expr, toType)
				else:
					instr = ir.Extend(expr, toType)
			else:
				instr = ir.Truncate(expr, toType)
		
		state.appendInstr(instr)
		
	def pretty(self, output, indent=0):
		self.expr.pretty(output, indent)
		output.write(' as ')
		if self.typeRef:
			self.typeRef.pretty(output)
		else:
			output.write(self.type.name)
