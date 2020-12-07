from .mir    import MIR, StaticData, StaticDataType, TypeModifiers
from ..      import ir
from ..types import allFields

class FieldInit:
	def __init__(self, access, field, offset=None):
		self.access = access
		self.field = field
		self.offset = offset if offset != None else field.offset

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
	failed = False
	if info.isStruct:
		baseOffset += field.offset
		type = field.type
		if type.isEnumType:
			type = type.structType
		for info in info.initInfo:
			field = type.fieldDict[info.name]
			failed = doInitField(state, inits, field, info, baseOffset) and not failed
		return
	
	access = state.analyzeNode(info.expr)
	if access:
		inits.append(FieldInit(access, field, baseOffset + field.offset))
		return failed
	else:
		return False

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
		
		failed = False
		for info in initInfo:
			field = structType.fieldDict[info.name]
			failed = doInitField(state, inits, field, info) and not failed
		
		if failed:
			return None
		
		return CreateStruct(inits, type, span)
	
	@staticmethod
	def init(name, expr):
		return FieldInitInfo(name, expr)
	
	@staticmethod
	def initStruct(name, initInfo):
		return StructInitInfo(name, initInfo)
	
	def commit(self, state):
		uninitFields = None if self.type.isEnumType else set(f for f in self.type.fields if not f.isUnionField)
		
		for init in self.inits:
			init.access.commit(state)
			
			if self.type.isEnumType:
				continue
			
			uninitFields.discard(init.field)
			uninitFields.update(TypeModifiers.getUninitFields(init.access))
		
		if uninitFields:
			flattened = set()
			for field in uninitFields:
				if field.type.isCompositeType:
					flattened.update(allFields(field.type))
				else:
					flattened.add(field)
			self.typeModifiers = TypeModifiers(False)
			self.typeModifiers.uninitFields = flattened
	
	def writeIR(self, state):
		fType = ir.FundamentalType.fromResolvedType(self.type)
		state.appendInstr(ir.Res(self, fType))
		
		for init in self.inits:
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
