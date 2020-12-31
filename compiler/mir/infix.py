from .mir       import MIR, StaticData, StaticDataType
from ..         import ir
from ..infixops import InfixOps, CMP_OPS

class InfixOp(MIR):
	def __init__(self, l, r, op, type, span):
		super().__init__(span, True)
		self.l = l
		self.r = r
		self.op = op
		self.type = type
	
	def staticEval(self, state):
		l = self.l.staticEval(state)
		if l == None:
			return None
		r = self.r.staticEval(state)
		if r == None:
			return None
		
		if l.dataType != StaticDataType.INT or r.dataType != StaticDataType.INT:
			return None
		
		rng = self.l.type.RNG
		fType = ir.FundamentalType.fromResolvedType(self.l.type)
		if self.op == InfixOps.PLUS:
			data = l.data + r.data
		elif self.op == InfixOps.MINUS:
			data = l.data - r.data
		elif self.op == InfixOps.TIMES:
			data = l.data * r.data
		elif self.op == InfixOps.DIV:
			data = l.data / r.data
		elif self.op == InfixOps.MODULO:
			data = l.data % r.data
		# elif self.op == InfixOps.LSHIFT:
		# 	data = l.data + r.data
		# elif self.op == InfixOps.RSHIFT:
		# 	data = l.data + r.data
		elif self.op == InfixOps.BITAND:
			data = l.data & r.data
		elif self.op == InfixOps.BITOR:
			data = l.data | r.data
		elif self.op == InfixOps.BITXOR:
			data = l.data ^ r.data
		elif self.op in CMP_OPS:
			rng = range(0, 2)
			fType = ir.I8
			if self.op == InfixOps.EQ:
				data = l.data == r.data
			elif self.op == InfixOps.NEQ:
				data = l.data != r.data
			elif self.op == InfixOps.GREATER:
				data = l.data > r.data
			elif self.op == InfixOps.LESS:
				data = l.data < r.data
			elif self.op == InfixOps.GREATEREQ:
				data = l.data >= r.data
			elif self.op == InfixOps.LESSEQ:
				data = l.data <= r.data
			else:
				assert 0
		else:
			return None
		
		if data not in rng:
			assert 0
		
		return StaticData(data, StaticDataType.INT, fType)
	
	def commit(self, state):
		self.l.commit(state)
		self.r.commit(state)
	
	def writeIR(self, state):
		if self.op == InfixOps.PLUS:
			self.writeAddIR(state)
		elif self.op == InfixOps.MINUS:
			self.writeSubIR(state)
		elif self.op == InfixOps.TIMES:
			self.writeMulIR(state)
		elif self.op == InfixOps.DIV:
			self.writeDivIR(state)
		elif self.op == InfixOps.MODULO:
			self.writeModuloIR(state)
		elif self.op == InfixOps.LSHIFT:
			self.writeLShiftIR(state)
		elif self.op == InfixOps.RSHIFT:
			self.writeRShiftIR(state)
		elif self.op == InfixOps.BITAND:
			self.writeBitAndIR(state)
		elif self.op == InfixOps.BITOR:
			self.writeBitOrIR(state)
		elif self.op == InfixOps.BITXOR:
			self.writeBitXOrIR(state)
		elif self.op in CMP_OPS:
			self.writeCmpIR(state)
		else:
			assert 0
	
	def writeAddIR(self, state):
		self.l.writeIR(state)
		
		if self.l.type.isPtrType:
			self.r.writeIR(state)
			if self.l.type.indLevel == 1:
				mul = self.l.type.baseType.byteSize
			else:
				mul = ir.IPTR.byteSize
			state.appendInstr(ir.Imm(self, ir.IPTR, mul))
			state.appendInstr(ir.Mul(self))
			state.appendInstr(ir.Add(self))
		elif self.r.type.isPtrType:
			if self.r.type.indLevel == 1:
				mul = self.r.type.baseType.byteSize
			else:
				mul = ir.IPTR.byteSize
			state.appendInstr(ir.Imm(self, ir.IPTR, mul))
			state.appendInstr(ir.Mul(self))
			self.r.writeIR(state)
			state.appendInstr(ir.Add(self))
		elif self.l.type.isFloatType:
			self.r.writeIR(state)
			state.appendInstr(ir.FAdd(self))
		else:
			self.r.writeIR(state)
			state.appendInstr(ir.Add(self))

	def writeSubIR(self, state):
		self.l.writeIR(state)
		
		if self.l.type.isPtrType:
			self.r.writeIR(state)
			if self.l.type.indLevel == 1:
				mul = self.l.type.baseType.byteSize
			else:
				mul = ir.IPTR.byteSize
			state.appendInstr(ir.Imm(self, ir.IPTR, mul))
			state.appendInstr(ir.Mul(self))
			state.appendInstr(ir.Sub(self))
		elif self.l.type.isFloatType:
			self.r.writeIR(state)
			state.appendInstr(ir.FSub(self))
		else:
			self.r.writeIR(state)
			state.appendInstr(ir.Sub(self))
	
	def writeMulIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		if self.l.type.isFloatType:
			state.appendInstr(ir.FMul(self))
		else:
			state.appendInstr(ir.Mul(self))
	
	def writeDivIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		if self.l.type.isFloatType:
			state.appendInstr(ir.FDiv(self))
		else:
			state.appendInstr(ir.Div(self))
	
	def writeModuloIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		state.appendInstr(ir.Mod(self))
	
	def writeLShiftIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		state.appendInstr(ir.LShift(self))
	
	def writeRShiftIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		if self.l.type.isSigned:
			state.appendInstr(ir.SRShift(self))
		else:
			state.appendInstr(ir.RShift(self))
	
	def writeBitAndIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		state.appendInstr(ir.BitAnd(self))
	
	def writeBitOrIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		state.appendInstr(ir.BitOr(self))
	
	def writeBitXOrIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		state.appendInstr(ir.BitXOr(self))
	
	def writeCmpIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		
		if self.op == InfixOps.EQ:
			if self.l.type.isFloatType:
				instr = ir.FEq(self)
			else:
				instr = ir.Eq(self)
		elif self.op == InfixOps.NEQ:
			if self.l.type.isFloatType:
				instr = ir.FNEq(self)
			else:
				instr = ir.NEq(self)
		elif self.op == InfixOps.GREATER:
			if self.l.type.isFloatType:
				instr = ir.FGreater(self)
			else:
				instr = ir.Greater(self)
		elif self.op == InfixOps.LESS:
			if self.l.type.isFloatType:
				instr = ir.FLess(self)
			else:
				instr = ir.Less(self)
		elif self.op == InfixOps.GREATEREQ:
			if self.l.type.isFloatType:
				instr = ir.FGreaterEq(self)
			else:
				instr = ir.GreaterEq(self)
		elif self.op == InfixOps.LESSEQ:
			if self.l.type.isFloatType:
				instr = ir.FLessEq(self)
			else:
				instr = ir.LessEq(self)
		else:
			assert 0
		
		state.appendInstr(instr)
	
	def __str__(self):
		return '{} {} {}'.format(str(self.l), self.op.value, str(self.r))
