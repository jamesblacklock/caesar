from .mir import MIR, StaticDataType
from ..ir import FundamentalType, Pop

class Coerce(MIR):
	def __init__(self, access, type, span):
		super().__init__(span, True)
		self.access = access
		self.type = type
	
	def checkFlow(self, scope):
		self.access.checkFlow(scope)
		if self.access.borrows:
			self.borrows = self.access.borrows
	
	def staticEval(self, state):
		staticValue = self.access.staticEval(state)
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
				logWarning(state, self.access.span, 'value out of range for type `{}` and will be clamped'
					.format(self.type))
			
			staticValue.fType = FundamentalType.fromResolvedType(self.type)
			return staticValue
		else:
			return None
	
	def writeTraitCoercionIR(self, state):
		vtblName = self.access.type.baseType.traitImpls[self.type.baseType].vtblName
		fType = FundamentalType.fromResolvedType(self.type)
		
		state.appendInstr(ir.Res(self, fType))
		
		self.access.writeIR(state)
		state.appendInstr(ir.Imm(self, ir.IPTR, 0))
		state.appendInstr(ir.FieldW(self, 2))
		
		state.appendInstr(ir.Global(self, ir.IPTR, vtblName))
		state.appendInstr(ir.Imm(self, ir.IPTR, ir.IPTR.byteSize))
		state.appendInstr(ir.FieldW(self, 2))
	
	def writeIR(self, state):
		fromType = self.access.type
		toType = self.type
		
		if fromType.isPtrType and not fromType.isTraitPtr and \
			toType.isPtrType and toType.isTraitPtr:
			self.writeTraitCoercionIR(state)
			return
		
		self.access.writeIR(state)
		
		if self.type.isVoidType:
			state.appendInstr(Pop(self))
			return
		
		fromSigned = self.access.type.isSigned
		toSigned = self.type.isSigned
		fromType = FundamentalType.fromResolvedType(fromType)
		toType = FundamentalType.fromResolvedType(toType)
		
		if fromType.byteSize == toType.byteSize and fromType.isFloatType == toType.isFloatType:
			assert 0
		
		instr = None
		if fromType.isFloatType and toType.isFloatType:
			if fromType.byteSize < toType.byteSize:
				instr = ir.FExtend(self, toType)
			else:
				instr = ir.FTruncate(self, toType)
		elif fromType.isFloatType:
			if toSigned:
				instr = ir.FToI(self, toType)
			else:
				instr = ir.FToU(self, toType)
		elif toType.isFloatType:
			if fromSigned:
				instr = ir.IToF(self, toType)
			else:
				instr = ir.UToF(self, toType)
		else:
			if fromType.byteSize < toType.byteSize:
				if toSigned:
					instr = ir.IExtend(self, toType)
				else:
					instr = ir.Extend(self, toType)
			else:
				instr = ir.Truncate(self, toType)
		
		state.appendInstr(instr)