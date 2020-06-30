from .ast        import AST
from .primitive  import IntLit
from ..          import infixops as ops, types
from ..infixops  import InfixOps
from ..types     import tryPromote, typesMatch, canAccommodate, hasDefiniteType
from ..log       import logError
from ..mir.infix import InfixOp as InfixOpMIR

class InfixOp(AST):
	def __init__(self, l, r, op, opTok, span):
		super().__init__(span, True)
		self.l = l
		self.r = r
		self.op = op
		self.opTok = opTok
	
	def analyze(infixOp, state, implicitType):
		def opErr(lType, rType):
			logError(state, infixOp.opTok.span, 'invalid operand types for operator `{}` ({} and {})'
				.format(infixOp.op.value, lType, rType))
		
		lIndefinite = not hasDefiniteType(infixOp.l)
		rIndefinite = not hasDefiniteType(infixOp.r)
		l = None
		r = None
		if lIndefinite and rIndefinite:
			assert type(infixOp.l) == IntLit and type(infixOp.r) == IntLit
			
			if canAccommodate(types.Int32, infixOp.l.value) and canAccommodate(types.Int32, infixOp.r.value):
				r = state.analyzeNode(infixOp.r, types.Int32)
				l = state.analyzeNode(infixOp.l, types.Int32)
			elif canAccommodate(types.Int64, infixOp.l.value) and canAccommodate(types.Int64, infixOp.r.value):
				r = state.analyzeNode(infixOp.r, types.Int64)
				l = state.analyzeNode(infixOp.l, types.Int64)
			else:
				r = state.analyzeNode(infixOp.r, types.UInt64)
				l = state.analyzeNode(infixOp.l, types.UInt64)
		elif rIndefinite and not infixOp.l.hasBlockValue:
			l = state.analyzeNode(infixOp.l, implicitType)
			if l:
				if type(infixOp.r) == IntLit and l.type and l.type.isPtrType:
					r = state.analyzeNode(infixOp.r, types.ISize if infixOp.r.value < 0 else types.USize)
				else:
					r = state.analyzeNode(infixOp.r, l.type)
		elif lIndefinite or infixOp.l.hasBlockValue:
			r = state.analyzeNode(infixOp.r, implicitType)
			if r:
				if type(infixOp.l) == IntLit and r.type and r.type.isPtrType:
					l = state.analyzeNode(infixOp.l, types.ISize if infixOp.l.value < 0 else types.USize)
				else:
					l = state.analyzeNode(infixOp.l, r.type)
		else:
			l = state.analyzeNode(infixOp.l, implicitType)
			r = state.analyzeNode(infixOp.r, l.type if l and l.type else implicitType)
		
		if not (l and l.type and r and r.type):
			return None
		
		l = tryPromote(state, l, r.type)
		r = tryPromote(state, r, l.type)
		
		lType = l.type
		rType = r.type
		
		resultType = None
		if lType == types.Bool and rType == types.Bool:
			if infixOp.op == InfixOps.EQ:
				resultType = types.Bool
		elif lType == types.Byte and rType == types.Byte:
			if infixOp.op == InfixOps.EQ:
				resultType = types.Bool
			if infixOp.op in ops.BITWISE_OPS:
				resultType = types.Byte
		elif lType == types.Char and rType == types.Char:
			if infixOp.op == InfixOps.EQ:
				resultType = types.Bool
		elif lType.isPtrType and rType.isIntType:
			if infixOp.op in ops.PTR_INT_OPS and rType.byteSize == types.USize.byteSize:
				resultType = lType
		elif lType.isIntType and rType.isPtrType:
			if infixOp.op in ops.INT_PTR_OPS and lType.byteSize == types.USize.byteSize:
				resultType = rType
		elif lType.isPtrType and rType.isPtrType:
			if infixOp.op in ops.PTR_PTR_OPS and typesMatch(lType, rType):
				resultType = lType
		elif typesMatch(lType, rType) and (lType.isIntType or lType.isFloatType):
			if infixOp.op == InfixOps.MODULO:
				if lType.isIntType:
					resultType = lType
			elif infixOp.op in ops.ARITHMETIC_OPS:
				resultType = lType
			elif (infixOp.op in ops.BITWISE_OPS or infixOp.op in ops.BITSHIFT_OPS) and lType.isIntType:
				resultType = lType
			elif infixOp.op in ops.CMP_OPS:
				resultType = types.Bool
		
		if resultType == None:
			opErr(lType, rType)
		
		return InfixOpMIR(l, r, infixOp.op, resultType, infixOp.span)
