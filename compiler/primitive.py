import re
from .ast       import ValueExpr, StaticData, StaticDataType
from .          import types, ir
from .log       import logError

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

def strUnesc(s):
	result = ''
	esc = False
	for c in s:
		if c == '\n':
			result += '\\n'
		elif c == '\r':
			result += '\\r'
		elif c == '\t':
			result += '\\t'
		elif c == '"':
			result += '\\"'
		else:
			result += c
	
	return result

def strBytes(s):
	b = [b for b in bytes(s, 'utf-8')]
	b.append(0)
	return b

STR_COUNTER = 0

class Label(ValueExpr):
	def __init__(self, label, resolvedType, span):
		self.label = label
		self.type = resolvedType
		self.span = span
	
	def writeIR(self, state):
		fType = ir.FundamentalType.fromResolvedType(self.type)
		state.appendInstr(ir.Static(self, fType, self.label))

class StrLit(ValueExpr):
	def __init__(self, value, span):
		super().__init__(span)
		self.value = strEsc(value)
		self.staticValue = None
		self.structLit = None
		self.cstr = False
	
	def analyze(lit, state, implicitType):
		from .structlit import StructLit, FieldLit
		
		global STR_COUNTER
		label = '{}_str{}'.format(state.scope.fnDecl.mangledName, STR_COUNTER)
		STR_COUNTER += 1
		
		lit.staticValue = StaticData(strBytes(lit.value), StaticDataType.BYTES, ir.IPTR, label)
		
		if lit.cstr:
			lit.type = types.PtrType(types.Byte, 1, False)
		else:
			class X: pass
			tok = X(); tok.content = 'Str'
			StrType = state.lookupSymbol([tok])
			tok.content = 'StrData'
			StrDataType = state.lookupSymbol([tok])
			
			size = IntLit(None, False, lit.span, value=len(lit.staticValue.data), type=types.USize)
			dataField = FieldLit(None, Label(lit.staticValue.label, types.USize, lit.span), lit.span, name='$Static')
			dataStruct = StructLit(None, True, [dataField], lit.span, typeSymbol=StrDataType.structType.fields[0].type)
			tagValue = IntLit(None, False, lit.span, value=StrDataType.symbolTable['Static'].tag.data, type=StrDataType.tagType)
			fields = [
				FieldLit(None, dataStruct, lit.span, name='$data'), 
				FieldLit(None, tagValue, lit.span, name='$tag')
			]
			data = StructLit(None, False, fields, lit.span, typeSymbol=StrDataType.structType)
			fields = [
				FieldLit(None, size, lit.span, name='size'), 
				FieldLit(None, data, lit.span, name='data')
			]
			lit.structLit = StructLit(None, False, fields, lit.span, typeSymbol=StrType)
			lit.type = lit.structLit.type
	
	def accessSymbols(self, scope):
		pass
	
	def writeIR(ast, state):
		state.staticDefs.append(ast.staticValue)
		
		if ast.cstr:
			state.appendInstr(ir.Static(ast, ir.IPTR, ast.staticValue.label))
		else:
			ast.structLit.writeIR(state)
	
	def pretty(self, output, indent=0):
		output.write('"{}"'.format(strUnesc(self.value)), indent)

class CharLit(ValueExpr):
	def __init__(self, value, span):
		super().__init__(span)
		bytes = strEsc(value)
		self.value = ord(bytes)
	
	def analyze(lit, state, implicitType):
		lit.type = types.Char
	
	def accessSymbols(self, scope):
		pass
	
	def writeIR(ast, state):
		state.appendInstr(ir.Imm(ast, ir.I32, ast.value))
	
	def pretty(self, output, indent=0):
		output.write('\'{}\''.format(strUnesc(chr(self.value))), indent)

class VoidLit(ValueExpr):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(lit, state, implicitType):
		lit.type = types.Void
	
	def accessSymbols(self, scope):
		pass
	
	def writeIR(lit, state):
		pass
	
	def pretty(self, output, indent=0):
		output.write('void', indent)

class BoolLit(ValueExpr):
	def __init__(self, value, span):
		super().__init__(span)
		self.value = value
	
	def analyze(lit, state, implicitType):
		lit.type = types.Bool
	
	def accessSymbols(self, scope):
		pass
	
	def writeIR(ast, state):
		state.appendInstr(ir.Imm(ast, ir.I8, 1 if ast.value else 0))
	
	def pretty(self, output, indent=0):
		output.write('true' if self.value else 'false', indent)

class IntLit(ValueExpr):
	def __init__(self, strValue, negate, span, value=None, type=None):
		super().__init__(span)
		if value != None:
			self.type = type
			base = 10
			suffix = None
		else:
			matches = re.match(r"^(0b|0x)?(.+?)(i8|u8|i16|u16|i32|u32|i64|u64|sz|usz)?$", strValue)
			
			base = 10
			if matches[1]:
				base = 2 if matches[1] == '0b' else 16
			
			value = int(matches[2].replace('_', ''), base)
			suffix = matches[3]
		
		self.base = base
		self.value = -value if negate else value
		self.suffix = suffix
	
	@staticmethod
	def const(value, type, span):
		return IntLit(None, False, span, value=value, type=type)
	
	def analyze(lit, state, implicitType):
		if lit.type:
			return
		
		if not implicitType or not implicitType.isIntLikeType or not types.canAccommodate(implicitType, lit.value):
			implicitType = None
		
		if lit.suffix == 'i8':
			lit.type = types.Int8
		elif lit.suffix == 'u8':
			lit.type = types.UInt8
		elif lit.suffix == 'i16':
			lit.type = types.Int16
		elif lit.suffix == 'u16':
			lit.type = types.UInt16
		elif lit.suffix == 'i32':
			lit.type = types.Int32
		elif lit.suffix == 'u32':
			lit.type = types.UInt32
		elif lit.suffix == 'i64':
			lit.type = types.Int64
		elif lit.suffix == 'u64':
			lit.type = types.UInt64
		elif lit.suffix == 'sz':
			lit.type = types.ISize
		elif lit.suffix == 'usz':
			lit.type = types.USize
		elif implicitType and implicitType.isPtrType:
			lit.type = types.ISize if lit.value < 0 else types.USize
		elif implicitType and implicitType.isIntLikeType and types.canAccommodate(implicitType, lit.value):
			lit.type = implicitType
		elif types.canAccommodate(types.Int32, lit.value):
			lit.type = types.Int32
		elif types.canAccommodate(types.Int64, lit.value) or lit.value < 0:
			lit.type = types.Int64
		else:
			lit.type = types.UInt64
		
		if not types.canAccommodate(lit.type, lit.value):
			logError(state, lit.span, 'integer value out of range for type {}'.format(lit.type))
	
	def accessSymbols(self, scope):
		pass
	
	def staticEval(self, state):
		fType = ir.FundamentalType.fromResolvedType(self.type)
		return StaticData(self.value, StaticDataType.INT, fType)
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Imm(ast, fType, ast.value))
	
	def pretty(self, output, indent=0):
		suffix = self.suffix if self.suffix else ''
		output.write(str(self.value) + suffix, indent)

class FloatLit(ValueExpr):
	def __init__(self, strValue, negate, span):
		super().__init__(span)
		matches = re.match(r"^(0x)?(.+?)(f32|f64)?$", strValue)
		valueStr = matches[2].replace('_', '').lower()
		
		if matches[1]:
			value = float.fromhex(valueStr)
		else:
			value = float(valueStr)
		
		suffix = matches[3]
		
		self.value = -value if negate else value
		self.suffix = suffix
	
	def analyze(lit, state, implicitType):
		if lit.suffix == 'f32':
			lit.type = types.Float32
		elif lit.suffix == 'f64':
			lit.type = types.Float64
		elif implicitType and implicitType.isFloatType:
			lit.type = implicitType
		else:# elif canAccommodate(types.Float32, lit.value):
			lit.type = types.Float32
		# else:
		# 	lit.type = types.Float64
		
		# if not canAccommodate(lit.type, lit.value):
		# 	logError(state, lit.span, 'floating point value out of range for type {}'.format(lit.type))
	
	def accessSymbols(self, scope):
		pass
	
	def writeIR(ast, state):
		fType = ir.FundamentalType.fromResolvedType(ast.type)
		state.appendInstr(ir.Imm(ast, fType, ast.value))
	
	def pretty(self, output, indent=0):
		suffix = self.suffix if self.suffix else ''
		output.write(str(self.value) + suffix, indent)
