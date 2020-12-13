from .mir import MIR
from ..ir import FundamentalType, FExtend, IExtend, Extend, Imm, Add, Deref, Call, Field, IPTR, F64

class FnCall(MIR):
	def __init__(self, access, args, cVarArgs, dynDispatch, type, span, isDrop=False):
		super().__init__(span, not isDrop)
		self.access = access
		self.args = args
		self.cVarArgs = cVarArgs
		self.isDrop = isDrop
		self.dynDispatch = dynDispatch
		self.type = type
	
	def commit(self, state):
		self.access.commit(state)
		for arg in self.args:
			arg.commit(state)
		for arg in self.cVarArgs:
			arg.commit(state)
	
	def writeInlineIR(self, state):
		symbols = [s for s in self.access.symbol.params]
		state.setupInlineCall(symbols)
		
		for block in self.access.symbol.cfg:
			block.writeIR(state, noReturn=True)
	
	def writeIR(self, state):
		for access in self.args:
			access.writeIR(state)
		
		# if self.access.symbol.inline:
		# 	assert not (self.cVarArgs or self.dynDispatch)
		# 	self.writeInlineIR(state)
		# 	return
		
		for access in self.cVarArgs:
			access.writeIR(state)
			if access.type.isVoidType:
				continue
			
			fType = FundamentalType.fromResolvedType(access.type)
			if fType.isFloatType and fType.byteSize != 8:
				assert fType.byteSize == 4
				state.appendInstr(FExtend(self, F64))
			elif fType.byteSize < 4:
				if access.type.isSigned:
					state.appendInstr(IExtend(self, IPTR))
				else:
					state.appendInstr(Extend(self, IPTR))
		
		if self.dynDispatch:
			selfArg = self.args[0]
			state.appendInstr(Imm(self, IPTR, IPTR.byteSize))
			state.appendInstr(Field(self, len(self.args), IPTR))
			index = 1 + selfArg.type.baseType.mod.symbols.index(self.access.symbol)
			state.appendInstr(Imm(self, IPTR, IPTR.byteSize * index))
			state.appendInstr(Add(self))
			state.appendInstr(Deref(self, IPTR))
		else:
			self.access.writeIR(state)
		
		argCt = len(self.args) + len(self.cVarArgs)
		retType = None if self.type.isVoidType else FundamentalType.fromResolvedType(self.type)
		state.appendInstr(Call(self, argCt, retType, self.access.type.cVarArgs))
	
	def __str__(self):
		access = str(self.access)
		args = ', '.join(str(arg) for arg in (*self.args, *self.cVarArgs))
		return '{}({})'.format(access, args)
