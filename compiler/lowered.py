

# class StructLit(ValueExpr):
		# if isConstExpr:
		# 	self.bytes = [0 for _ in range(0, self.type.byteSize)]
		# 	for (name, init) in fieldInits.items():
		# 		offset = self.type.fieldDict[name].offset
		# 		end = offset + len(init.bytes)
		# 		self.bytes[offset : end] = init.bytes

# class Coercion(ValueExpr):
		# if self.expr.isConstExpr:
		# 	self.isConstExpr = self.expr.isConstExpr
		# 	if self.type.isVoidType and self.type.isPrimitiveType:
		# 		self.bytes = []
		# 	elif self.expr.type.isIntType and self.type.isFloatType:
		# 		assert 0
		# 	elif self.expr.type.isFloatType and self.type.isIntType:
		# 		assert 0
		# 	elif (self.expr.type.isIntType or self.expr.type.isPtrType) and \
		# 		(self.type.isIntType or self.type.isPtrType):
		# 		if self.type.byteSize < self.expr.type.byteSize:
		# 			self.bytes = self.expr.bytes[:self.type.byteSize]
		# 		elif self.type.byteSize > self.expr.type.byteSize:
		# 			count = self.type.byteSize - self.expr.type.byteSize
		# 			b = 0xff if self.expr.value < 0 else 0x00
		# 			self.bytes = list(self.expr.bytes)
		# 			for _ in range(0, count): self.bytes.append(b)
		# 		else:
		# 			self.bytes = self.expr.bytes
		# 	elif self.expr.type.isFloatType and self.type.isFloatType:
		# 		assert 0
		# 	else:
		# 		assert 0
