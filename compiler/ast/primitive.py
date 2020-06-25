import re
from .ast            import ValueExpr
from ..log           import logError
from ..mir.primitive import VoidValue, BoolValue, IntValue, FloatValue
from ..              import types

class VoidLit(ValueExpr):
	def __init__(self, span):
		super().__init__(span)
	
	def analyze(self, state, implicitType):
		return VoidValue(self.span)

class BoolLit(ValueExpr):
	def __init__(self, value, span):
		super().__init__(span)
		self.value = value
	
	def analyze(self, state, implicitType):
		return BoolValue(self.value, self.span)

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
	
	def analyze(self, state, implicitType):
		if not implicitType or not implicitType.isIntLikeType or not types.canAccommodate(implicitType, self.value):
			implicitType = None
		
		if self.suffix == 'i8':
			type = types.Int8
		elif self.suffix == 'u8':
			type = types.UInt8
		elif self.suffix == 'i16':
			type = types.Int16
		elif self.suffix == 'u16':
			type = types.UInt16
		elif self.suffix == 'i32':
			type = types.Int32
		elif self.suffix == 'u32':
			type = types.UInt32
		elif self.suffix == 'i64':
			type = types.Int64
		elif self.suffix == 'u64':
			type = types.UInt64
		elif self.suffix == 'sz':
			type = types.ISize
		elif self.suffix == 'usz':
			type = types.USize
		elif implicitType and implicitType.isPtrType:
			type = types.ISize if self.value < 0 else types.USize
		elif implicitType and implicitType.isIntLikeType and types.canAccommodate(implicitType, self.value):
			type = implicitType
		elif types.canAccommodate(types.Int32, self.value):
			type = types.Int32
		elif types.canAccommodate(types.Int64, self.value) or self.value < 0:
			type = types.Int64
		else:
			type = types.UInt64
		
		if not types.canAccommodate(type, self.value):
			logError(state, self.span, 'integer value out of range for type {}'.format(type))
		
		return IntValue(self.value, type, self.span)

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
	
	def analyze(self, state, implicitType):
		if self.suffix == 'f32':
			type = types.Float32
		elif self.suffix == 'f64':
			type = types.Float64
		elif implicitType and implicitType.isFloatType:
			type = implicitType
		else:# elif canAccommodate(types.Float32, self.value):
			type = types.Float32
		# else:
		# 	self.type = types.Float64
		
		# if not canAccommodate(self.type, self.value):
		# 	logError(state, self.span, 'floating point value out of range for type {}'.format(self.type))
		
		return FloatValue(self.value, type, self.span)
