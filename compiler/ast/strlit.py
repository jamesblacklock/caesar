from .ast               import ValueExpr
from ..mir.label        import Label
from ..mir.createstruct import CreateStruct
from ..                 import types
from ..mir.mir          import StaticData, StaticDataType
from ..mir.primitive    import IntValue
from ..ir               import IPTR

def strEsc(s):
	result = ''
	esc = False
	for c in s[1:-1]:
		if c == '\\':
			esc = True
		elif esc:
			esc = False
			if c == 'n':
				result += '\n'
			elif c == 'r':
				result += '\r'
			elif c == 't':
				result += '\t'
			elif c == '"':
				result += c
			else:
				result += '\\' + c
		else:
			result += c
	
	return result

def strBytes(s):
	b = [b for b in bytes(s, 'utf-8')]
	b.append(0)
	return b

class CharLit(ValueExpr):
	def __init__(self, value, span):
		super().__init__(span)
		bytes = strEsc(value)
		self.value = ord(bytes)
	
	def analyze(self, state, implicitType):
		self.type = types.Char

STR_COUNTER = 0

class StrLit(ValueExpr):
	def __init__(self, value, span):
		super().__init__(span)
		self.value = value
	
	def analyze(self, state, implicitType):
		global STR_COUNTER
		label = '{}_str{}'.format(state.scope.fnDecl.mangledName, STR_COUNTER)
		STR_COUNTER += 1
		
		value = strEsc(self.value)
		staticData = StaticData(strBytes(value), StaticDataType.BYTES, IPTR, label)
		bytePtrType = types.PtrType(types.Byte, 1, False)
		label = Label(staticData.label, bytePtrType, staticData, self.span)
		
		if implicitType and types.typesMatch(implicitType, bytePtrType):
			return state.analyzeNode(label)
		
		StrType = state.strMod.symbolTable['str']
		StrDataType = state.strMod.symbolTable['StrData']
		
		size = len(staticData.data)
		tagValue = StrDataType.symbolTable['Static'].tag.data
		structValue = CreateStruct.create(state, StrType, self.span, [
			CreateStruct.init('size', IntValue(size, types.USize, self.span)),
			CreateStruct.initStruct('data', [
				CreateStruct.initStruct('$data', [CreateStruct.init('$Static', label)]),
				CreateStruct.init('$tag', IntValue(tagValue, types.UInt8, self.span))
			])
		])
		
		return structValue#Str(self.value, structValue, StrType, self.span)
