from .mir      import MIR
from ..ir      import FAdd, FSub, FMul, FDiv, FEq, FNEq, FGreater, FLess, FGreaterEq, FLessEq, \
                      Add, Sub, Mul, Div, Mod, Eq, NEq, Greater, Less, GreaterEq, LessEq, Imm, IPTR
from ..ast.ast import InfixOps, CMP_OPS

class InfixOp(MIR):
	def __init__(self, l, r, op, type, span):
		super().__init__(span, True)
		self.l = l
		self.r = r
		self.op = op
		self.type = type
	
	def checkFlow(self, scope):
		self.l.checkFlow(scope)
		self.r.checkFlow(scope)
	
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
				mul = IPTR.byteSize
			state.appendInstr(Imm(self, IPTR, mul))
			state.appendInstr(Mul(self))
			state.appendInstr(Add(self))
		elif self.r.type.isPtrType:
			if self.r.type.indLevel == 1:
				mul = self.r.type.baseType.byteSize
			else:
				mul = IPTR.byteSize
			state.appendInstr(Imm(self, IPTR, mul))
			state.appendInstr(Mul(self))
			self.r.writeIR(state)
			state.appendInstr(Add(self))
		elif self.l.type.isFloatType:
			self.r.writeIR(state)
			state.appendInstr(FAdd(self))
		else:
			self.r.writeIR(state)
			state.appendInstr(Add(self))

	def writeSubIR(self, state):
		self.l.writeIR(state)
		
		if self.l.type.isPtrType:
			self.r.writeIR(state)
			if self.l.type.indLevel == 1:
				mul = self.l.type.baseType.byteSize
			else:
				mul = IPTR.byteSize
			state.appendInstr(Imm(self, IPTR, mul))
			state.appendInstr(Mul(self))
			state.appendInstr(Sub(self))
		elif self.l.type.isFloatType:
			self.r.writeIR(state)
			state.appendInstr(FSub(self))
		else:
			self.r.writeIR(state)
			state.appendInstr(Sub(self))
	
	def writeMulIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		if self.l.type.isFloatType:
			state.appendInstr(FMul(self))
		else:
			state.appendInstr(Mul(self))
	
	def writeDivIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		if self.l.type.isFloatType:
			state.appendInstr(FDiv(self))
		else:
			state.appendInstr(Div(self))
	
	def writeModuloIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		state.appendInstr(Mod(self))
	
	def writeCmpIR(self, state):
		self.l.writeIR(state)
		self.r.writeIR(state)
		
		if self.op == InfixOps.EQ:
			if self.l.type.isFloatType:
				instr = FEq(self)
			else:
				instr = Eq(self)
		elif self.op == InfixOps.NEQ:
			if self.l.type.isFloatType:
				instr = FNEq(self)
			else:
				instr = NEq(self)
		elif self.op == InfixOps.GREATER:
			if self.l.type.isFloatType:
				instr = FGreater(self)
			else:
				instr = Greater(self)
		elif self.op == InfixOps.LESS:
			if self.l.type.isFloatType:
				instr = FLess(self)
			else:
				instr = Less(self)
		elif self.op == InfixOps.GREATEREQ:
			if self.l.type.isFloatType:
				instr = FGreaterEq(self)
			else:
				instr = GreaterEq(self)
		elif self.op == InfixOps.LESSEQ:
			if self.l.type.isFloatType:
				instr = FLessEq(self)
			else:
				instr = LessEq(self)
		else:
			assert 0
		
		state.appendInstr(instr)
	
	def __str__(self):
		return '{} {} {}'.format(str(self.l), self.op.value, str(self.r))
