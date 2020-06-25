from .mir    import MIR, StaticData, StaticDataType
from ..types import typesMatch, Void, TupleType, ArrayType
from ..      import ir

class FieldInit:
	def __init__(self, access, offset):
		self.access = access
		self.offset = offset

class FieldInitInfo:
	def __init__(self, name, expr):
		self.name = name
		self.expr = expr
		self.isStruct = False

class StructInitInfo:
	def __init__(self, name, initInfo):
		self.name = name
		self.initInfo = initInfo
		self.isStruct = True

def doInitField(state, inits, field, info, baseOffset=0):
	if info.isStruct:
		baseOffset += field.offset
		type = field.type
		if type.isEnumType:
			type = type.structType
		for info in info.initInfo:
			field = type.fieldDict[info.name]
			doInitField(state, inits, field, info, baseOffset)
		return
	
	access = state.analyzeNode(info.expr)
	inits.append(FieldInit(access, baseOffset + field.offset))

class CreateStruct(MIR):
	def __init__(self, inits, type, span):
		super().__init__(span, True)
		self.inits = inits
		self.type = type
	
	@staticmethod
	def create(state, type, span, initInfo):
		inits = []
		structType = type
		if structType.isEnumType:
			structType = structType.structType
		
		for info in initInfo:
			field = structType.fieldDict[info.name]
			doInitField(state, inits, field, info)
		
		return CreateStruct(inits, type, span)
	
	@staticmethod
	def init(name, expr):
		return FieldInitInfo(name, expr)
	
	@staticmethod
	def initStruct(name, initInfo):
		return StructInitInfo(name, initInfo)
	
	def checkFlow(self, scope):
		for init in self.inits:
			init.access.checkFlow(scope)
	
	def writeIR(self, state):
		fType = ir.FundamentalType.fromResolvedType(self.type)
		state.appendInstr(ir.Res(self, fType))
		
		# if lit.type.isStructType:
		# 	state.initStructFields(lit, baseOffset)
		# 	return
		
		for init in self.inits:
			# if init.type.isCompositeType:
			# 	state.initCompositeFields(init, baseOffset + fieldInfo.offset)
			# 	continue
			
			init.access.writeIR(state)
			state.appendInstr(ir.Imm(init.access, ir.IPTR, init.offset))
			state.appendInstr(ir.FieldW(self, 2))
	
	def staticEval(self, state):
		structBytes = [0 for _ in range(0, self.type.byteSize)]
		for init in self.inits:
			staticFieldValue = init.access.staticEval(state)
			if staticFieldValue == None:
				return None
			
			fieldBytes = staticFieldValue.toBytes()
			end = init.offset + len(fieldBytes)
			structBytes[init.offset : end] = fieldBytes
		
		fType = ir.FundamentalType.fromResolvedType(self.type)
		return StaticData(structBytes, StaticDataType.BYTES, fType)
	
	def __str__(self):
		if len(self.inits) == 0:
			return 'struct {}'
		
		lines = ['struct']
		for init in self.inits:
			lines.append('\t[{}]: {}'.format(init.offset, str(init.access)))
		return '\n'.join(lines)
