from .mir       import MIR
from ..         import ir
from ..infixops import InfixOps, CMP_OPS

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
