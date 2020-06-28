from enum        import Enum
from ..token     import TokenType
from .ast        import AST, InfixOps,ARITHMETIC_OPS, BITWISE_OPS, BITSHIFT_OPS, CMP_OPS, \
                        LOGIC_OPS, PTR_PTR_OPS, PTR_INT_OPS, INT_PTR_OPS, RNG_OPS
from .primitive  import IntLit
from .block      import Block
from .ifexpr     import If
from ..types     import tryPromote, typesMatch, canAccommodate, hasDefiniteType, \
                        Int32, Int64, UInt64, ISize, USize, Bool, Byte, Char
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
			if type(infixOp.l) == IntLit and type(infixOp.r) == IntLit:
				if canAccommodate(Int32, infixOp.l.value) and \
					canAccommodate(Int32, infixOp.r.value):
					r = state.analyzeNode(infixOp.r, Int32)
					l = state.analyzeNode(infixOp.l, Int32)
				elif canAccommodate(Int64, infixOp.l.value) and \
					canAccommodate(Int64, infixOp.r.value):
					r = state.analyzeNode(infixOp.r, Int64)
					l = state.analyzeNode(infixOp.l, Int64)
				else:
					r = state.analyzeNode(infixOp.r, UInt64)
					l = state.analyzeNode(infixOp.l, UInt64)
			else:
				assert 0
		elif rIndefinite and type(infixOp.l) not in (Block, If):
			l = state.analyzeNode(infixOp.l, implicitType)
			if l:
				if type(infixOp.r) == IntLit and l.type and l.type.isPtrType:
					# if infixOp.op in PTR_INT_OPS:
						r = state.analyzeNode(infixOp.r, ISize if infixOp.r.value < 0 else USize)
					# else:
					# 	opErr()
					# 	return
				else:
					r = state.analyzeNode(infixOp.r, l.type)
		elif lIndefinite or type(infixOp.l) in (Block, If):
			r = state.analyzeNode(infixOp.r, implicitType)
			if r:
				if type(infixOp.l) == IntLit and r.type and r.type.isPtrType:
					# if infixOp.op in PTR_INT_OPS:
						l = state.analyzeNode(infixOp.l, ISize if infixOp.l.value < 0 else USize)
					# else:
						# opErr()
						# return
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
		if lType == Bool and rType == Bool:
			if infixOp.op == InfixOps.EQ:
				resultType = Bool
		elif lType == Byte and rType == Byte:
			if infixOp.op == InfixOps.EQ:
				resultType = Bool
			if infixOp.op in BITWISE_OPS:
				resultType = Byte
		elif lType == Char and rType == Char:
			if infixOp.op == InfixOps.EQ:
				resultType = Bool
		elif lType.isPtrType and rType.isIntType:
			if infixOp.op in PTR_INT_OPS and rType.byteSize == USize.byteSize:
				resultType = lType
		elif lType.isIntType and rType.isPtrType:
			if infixOp.op in INT_PTR_OPS and lType.byteSize == USize.byteSize:
				resultType = rType
		elif lType.isPtrType and rType.isPtrType:
			if infixOp.op in PTR_PTR_OPS and typesMatch(lType, rType):
				resultType = lType
		elif typesMatch(lType, rType) and (lType.isIntType or lType.isFloatType):
			if infixOp.op == InfixOps.MODULO:
				if lType.isIntType:
					resultType = lType
			elif infixOp.op in ARITHMETIC_OPS:
				resultType = lType
			elif (infixOp.op in BITWISE_OPS or infixOp.op in BITSHIFT_OPS) and lType.isIntType:
				resultType = lType
			elif infixOp.op in CMP_OPS:
				resultType = Bool
		
		if resultType == None:
			opErr(lType, rType)
		
		return InfixOpMIR(l, r, infixOp.op, resultType, infixOp.span)
