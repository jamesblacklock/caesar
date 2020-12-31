from .ast               import AST
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

class CharLit(AST):
	def __init__(self, value, span):
		super().__init__(span, True)
		bytes = strEsc(value)
		self.value = ord(bytes)
	
	def analyze(self, state, implicitType):
		return IntValue(self.value, types.Char, self.span)

STR_COUNTER = 0

class StrLit(AST):
	def __init__(self, value, span):
		super().__init__(span, True)
		self.value = value
	
	def analyze(self, state, implicitType):
		global STR_COUNTER
		label = '{}_str{}'.format(state.fn.mangledName, STR_COUNTER)
		STR_COUNTER += 1
		
		value = strEsc(self.value)
		staticData = StaticData(strBytes(value), StaticDataType.BYTES, IPTR, label)
		bytePtrType = types.PtrType(types.Byte, 1, False)
		label = Label(staticData.label, bytePtrType, staticData, self.span)
		
		if not state.mod.strMod or implicitType and types.typesMatch(implicitType, bytePtrType):
			return state.analyzeNode(label)
		
		size = len(staticData.data)
		tagValue = state.mod.strMod.StrData.type.symbolTable['Static'].tag.data
		structValue = CreateStruct.create(state, state.mod.strMod.Str.type, self.span, [
			CreateStruct.init('size', IntValue(size, types.USize, self.span)),
			CreateStruct.initStruct('data', [
				CreateStruct.initStruct('$data', [CreateStruct.init('$Static', label)]),
				CreateStruct.init('$tag', IntValue(tagValue, types.UInt8, self.span))
			])
		])
		
		return structValue
