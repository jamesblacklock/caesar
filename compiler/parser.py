import re
from os                  import path
from enum                import Enum
from .token              import TokenType
from .span               import Span, revealSpan, AnsiColor
from .err                import logError
from .ast                import CConv, FnDeclAST, LetAST, FnCallAST, ReturnAST, IfAST, TypeRefAST, \
                                AttrAST, StrLitAST, IntLitAST, FloatLitAST, BoolLitAST, TupleLitAST, \
                                ValueRefAST, InfixOpAST, FnCallAST, IfAST, CoercionAST, ModAST, \
                                ValueExprAST, FnParamAST, BlockAST, AsgnAST, WhileAST, DerefAST, \
                                IndexOpAST, VoidAST, CVarArgsParamAST, InfixOp, AddressAST, LoopAST, \
                                BreakAST, ContinueAST, CharLitAST, FieldDeclAST, StructDeclAST, \
                                FieldLitAST, StructLitAST, FieldAccessAST, INFIX_PRECEDENCE, \
                                SignAST, UNARY_PRECEDENCE

class ParserState:
	def __init__(self, source, tokens):
		self.source            = source
		self.tokens            = tokens
		self._offset           = 0
		self.tok               = self.tokens[0]
		self.nextTok           = self.tokens[1] if len(self.tokens) > 1 else self.tok
		self.eof               = self.tok.type == TokenType.EOF
		self.indentLevels      = [0]
		self.failed            = False
	
	def advance(self):
		if self.eof:
			return
		
		self._offset += 1
		self.tok = self.tokens[self._offset]
		self.eof = self.tok.type == TokenType.EOF
		self.nextTok = self.tok if self.eof else self.tokens[self._offset+1]
	
	@property
	def offset(self):
		return self._offset
	
	@property
	def indentLevel(self):
		return self.indentLevels[-1]
		
	def pushIndentLevel(self, level):
		self.indentLevels.append(level)
	
	def popIndentLevel(self):
		assert len(self.indentLevels) > 1
		return self.indentLevels.pop()
	
	def rollback(self, offset):
		self._offset = offset
		self.tok = self.tokens[self._offset]
		self.eof = self.tok.type == TokenType.EOF
		self.nextTok = self.tok if self.eof else self.tokens[self._offset+1]
	
	def skip(self, *types):
		skipped = False
		while self.tok.type in types:
			self.advance()
			skipped = True
		return skipped
	
	def skipEmptyLines(self):
		line = self.tok.span.startLine
		while True:
			self.skip(TokenType.NEWLINE, TokenType.SPACE, TokenType.COMMENT)
			if self.tok.type == TokenType.INDENT:
				offset = self.offset
				self.skip(TokenType.INDENT, TokenType.SPACE, TokenType.COMMENT)
				if self.tok.type == TokenType.NEWLINE or self.tok.type == TokenType.EOF:
					continue
				else:
					self.rollback(offset)
			break
		return self.tok.span.startLine > line
	
	def skipSpace(self):
		return self.skip(TokenType.SPACE, TokenType.COMMENT)
	
	def skipUntil(self, *types):
		skipped = False
		while self.tok.type != TokenType.EOF and self.tok.type not in types:
			self.advance()
			skipped = True
		return skipped

def expectIndent(state):
	assert state.tok.span.startColumn == 1
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
		state.advance()
		if state.tok.type == TokenType.COMMENT:
			state.skipSpace()
	
	level = 0 if indentTok == None else len(indentTok.content)
	if level != state.indentLevel:
		logError(state, state.tok.span, 'expected indent level {}, found level {}'
				.format(state.indentLevel, level))
		return False
	
	return True

def isIndentIncrease(state):
	if state.tok.span.startColumn != 1:
		return False
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
	
	level = 0 if indentTok == None else len(indentTok.content)
	return level > state.indentLevel

def isIndentDecrease(state):
	if state.tok.span.startColumn != 1:
		return False
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
	
	level = 0 if indentTok == None else len(indentTok.content)
	return level < state.indentLevel

def expectIndentIncrease(state):
	assert state.tok.span.startColumn == 1
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
		state.advance()
		if state.tok.type == TokenType.COMMENT:
			state.skipSpace()
	
	level = 0 if indentTok == None else len(indentTok.content)
	if level <= state.indentLevel:
		logError(state, state.tok.span, 'expected indented block')
		return False
	else:
		state.pushIndentLevel(level)
		return True

def expectType(state, *types):
	if state.tok.type not in types:
		typesStr = ', '.join([type.desc() for type in types[0:-1]]) + \
			(',' if len(types) > 2 else '') + (' or ' if len(types) > 1 else '') + types[-1].desc()
		logError(state, state.tok.span, 'expected {}, found {}'
				.format(typesStr, state.tok.type.desc()))
		return False
	else:
		return True

def permitLineBreak(state):
	if state.skipEmptyLines():
		return expectIndent(state)
	return False

def permitLineBreakIndent(state):
	if state.skipEmptyLines():
		return expectIndentIncrease(state)
	return False

def expectLineBreak(state):
	if state.skipEmptyLines() == False:
		logError(state, state.tok.span, 'expected line break, found {}'.format(state.tok.type.desc()))
		return False
	return expectIndent(state)

def parseAttrArgs(state):
	def parseAttrArg(state):
		if expectType(state, TokenType.STRING, TokenType.INTEGER) == False:
			return None
		arg = state.tok
		state.advance()
		return arg
	
	return parseBlock(state, parseAttrArg, BlockMarkers.PAREN, True)

def parseAttrs(state):
	attrs = []
	
	while state.tok.type == TokenType.AT:
		span = state.tok.span
		state.advance()
		
		if expectType(state, TokenType.NAME) == False:
			break
		
		span = Span.merge(span, state.tok.span)
		name = state.tok.content
		args = []
		state.advance()
		state.skipSpace()
		
		if state.tok.type == TokenType.NEWLINE:
			state.skipEmptyLines()
			expectIndent(state)
		
		if state.tok.type == TokenType.LPAREN:
			block = parseAttrArgs(state)
			args = block.list
			span = Span.merge(span, block.span)
		
		attrs.append(AttrAST(name, args, span))
		
		if state.skipEmptyLines():
			expectIndent(state)
	
	return attrs

def parseFnDeclParams(state):
	def parseFnParam(state):
		if state.tok.type == TokenType.ELLIPSIS:
			ret = CVarArgsParamAST(state.tok.span)
			state.advance()
			if state.tok.type != TokenType.RPAREN:
				logError(state, ret.span, 'C variadic parameter must come last')
				return None
			
			return ret
		
		if expectType(state, TokenType.NAME) == False:
			return None
		
		name = state.tok.content
		span = state.tok.span
		
		state.advance()
		state.skipSpace()
		
		if expectType(state, TokenType.COLON) == False:
			return FnParamAST(name, None, span)
		
		Span.merge(span, state.tok.span)
		
		state.advance()
		onOneLine = permitLineBreakIndent(state) == False
		state.skipSpace()
		
		typeRef = parseTypeRef(state)
		if typeRef:
			Span.merge(span, typeRef.span)
		
		if not onOneLine:
			state.popIndentLevel()
		
		return FnParamAST(name, typeRef, span)
	
	return parseBlock(state, parseFnParam, BlockMarkers.PAREN, True)

def parseFnDeclReturnType(state):
	state.advance()
	onOneLine = permitLineBreakIndent(state) == False
	state.skipSpace()
	
	typeRef = parseTypeRef(state)
	
	if not onOneLine:
		state.popIndentLevel()
	
	return typeRef

def parseStructDecl(state, doccomment, attrs, anon):
	def parseFieldDecl(state):
		span = state.tok.span
		
		fieldAttrs = []
		if state.tok.type == TokenType.AT:
			fieldAttrs = parseAttrs(state)
			permitLineBreak(state)
		
		if expectType(state, TokenType.NAME) == False:
			return None
		
		nameTok = state.tok
		
		state.advance()
		state.skipSpace()
		
		if expectType(state, TokenType.COLON) == False:
			return FieldDeclAST(nameTok, None, span)
		
		span = Span.merge(span, state.tok.span)
		
		state.advance()
		onOneLine = permitLineBreakIndent(state) == False
		state.skipSpace()
		
		typeRef = parseTypeRef(state)
		if typeRef:
			span = Span.merge(span, typeRef.span)
		
		if not onOneLine:
			state.popIndentLevel()
		
		return FieldDeclAST(fieldAttrs, nameTok, typeRef, span)
	
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	name = None
	if not anon and expectType(state, TokenType.NAME):
		name = state.tok.content
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
	
	block = parseBlock(state, parseFieldDecl)
	span = Span.merge(span, block.span)
	return StructDeclAST(doccomment, attrs, name, block.list, span)

def parseCoercion(state, expr):
	span = Span.merge(expr.span, state.tok.span)
	state.advance() # skip `as`
	onOneLine = permitLineBreakIndent(state) == False
	state.skipSpace()
	
	typeRef = parseTypeRef(state)
	
	if not onOneLine:
		state.popIndentLevel()
	
	return CoercionAST(expr, typeRef, Span.merge(span, typeRef.span))

class BlockMarkers(Enum):
	PAREN = 'PAREN'
	BRACE = 'BRACE'
	BRACK = 'BRACK'

class Block:
	def __init__(self, list, span, trailingSeparator=False):
		self.list = list
		self.span = span
		self.trailingSeparator = trailingSeparator

def parseBlock(state, parseItem, blockMarkers=BlockMarkers.BRACE, 
	requireBlockMarkers=False, topLevelBlock=False):
	
	# set the marker token types
	if blockMarkers == BlockMarkers.BRACE:
		openMarker = TokenType.LBRACE
		closeMarker = TokenType.RBRACE
	elif blockMarkers == BlockMarkers.BRACK:
		openMarker = TokenType.LBRACK
		closeMarker = TokenType.RBRACK
	else:
		openMarker = TokenType.LPAREN
		closeMarker = TokenType.RPAREN
	
	
	# advance to the open marker (if it exists)
	state.skipSpace()
	needsTerm = False
	indentedBlock = False
	unindented = False
	startSpan = endSpan = state.tok.span
	
	if not topLevelBlock:
		if state.tok.type == openMarker:
			# open marker appears on the same line
			needsTerm = True
			state.advance()
		elif state.skipEmptyLines():
			if state.tok.type == openMarker or \
				state.tok.type == TokenType.INDENT and state.nextTok.type == openMarker:
				# open marker appears on the next line at the same indent level
				needsTerm = True
				expectIndent(state)
				startSpan = endSpan = state.tok.span
				state.advance() # move past open marker
			else:
				# no open marker present, just a newline-marked block
				startSpan = endSpan = state.tok.span
				indentedBlock = expectIndentIncrease(state)
				if not indentedBlock:
					unindented = True
		else:
			# we found something other than a block here
			if requireBlockMarkers:
				assert expectType(state, openMarker) == False
			else:
				assert expectType(state, TokenType.NEWLINE, openMarker) == False
			return Block([], startSpan, False)
	
	# if block marker is required, we should have found it by now
	if requireBlockMarkers and not needsTerm:
		assert expectType(state, openMarker) == False
		state.skipUntil(TokenType.NEWLINE)
		return Block([], startSpan, False)
	
	
	trailingSeparator = False
	list = []
	while not unindented:
		# check to see if the block was terminated
		offset = state.offset
		if needsTerm:
			state.skipSpace()
			if state.tok.type == closeMarker or state.tok.type == TokenType.EOF:
				# close marker on the same line
				expectType(state, closeMarker)
				endSpan = state.tok.span
				state.advance()
				break
			elif state.tok.type == TokenType.NEWLINE:
				# maybe a close marker on the next line
				state.skipEmptyLines()
				if state.tok.type == closeMarker or \
					state.tok.type == TokenType.INDENT and state.nextTok.type == closeMarker:
					# close marker on next line confirmed!
					state.popIndentLevel()
					unindented = True
					expectIndent(state)
					endSpan = state.tok.span
					state.advance()
					break
		else:
			state.skipEmptyLines()
			if isIndentDecrease(state) or state.tok.type == TokenType.EOF:
				# decreased indent level on the next line or end of file
				state.rollback(offset)
				break
		
		# no close marker, rollback
		state.rollback(offset)
		
		# check if we need to increase the indent level (only happens once in a block)
		if state.skipEmptyLines():
			if not indentedBlock and not topLevelBlock:
				indentedBlock = expectIndentIncrease(state)
				if not indentedBlock:
					unindented = True
					break
			else:
				expectIndent(state)
		
		# space cannot precede the first expression on a line unless the space is 
		# preceded by a comment (otherwise it would appear to be indentation)
		if not indentedBlock or state.tok.type == TokenType.COMMENT:
			state.skipSpace()
		elif state.tok.type == TokenType.SPACE:
			logError(state, state.tok.span, 'expected expression, found space')
		
		# parse an item in the block
		trailingSeparator = False
		item = parseItem(state)
		if item == None:
			# if item parsing failed, try to skip to a sane place
			skipUntilTypes = (TokenType.COMMA, TokenType.SEMICOLON, TokenType.NEWLINE, closeMarker) \
				if needsTerm else (TokenType.COMMA, TokenType.SEMICOLON, TokenType.NEWLINE)
			state.skipUntil(*skipUntilTypes)
		else:
			list.append(item)
			endSpan = item.span
		
		# following the item we expect a comma or a close marker 
		# (or newline/eof if the block needs no close marker)
		state.skipSpace()
		if needsTerm:
			if expectType(state, TokenType.INDENT, TokenType.COMMA, TokenType.SEMICOLON, \
				TokenType.NEWLINE, closeMarker) == False:
				state.skipUntil(closeMarker)
		else:
			if expectType(state, TokenType.INDENT, TokenType.COMMA, TokenType.SEMICOLON, \
				TokenType.NEWLINE, TokenType.EOF) == False:
				break
		
		# skip the comma (it's just a separator)
		if state.tok.type == TokenType.COMMA or state.tok.type == TokenType.SEMICOLON:
			state.advance()
			trailingSeparator = True
	
	if indentedBlock and not unindented:
		state.popIndentLevel()
	
	return Block(list, Span.merge(startSpan, endSpan), trailingSeparator)

def parseTypeRef(state):
	if expectType(state, TokenType.NAME, TokenType.VOID) == False:
		return None
	
	path = parsePath(state)
	span = path.span
	indirectionLevel = 0
	
	state.skipSpace()
	while state.tok.type == TokenType.AMP or state.tok.type == TokenType.AND:
		span = Span.merge(span, state.tok.span)
		indirectionLevel += 1 if state.tok.type == TokenType.AMP else 2
		state.advance()
		state.skipSpace()
	
	return TypeRefAST(path.path, indirectionLevel, span)

def parseFnCall(state, expr):
	block = parseBlock(state, parseValueExpr, BlockMarkers.PAREN, True)
	return FnCallAST(expr, block.list, Span.merge(expr.span, block.span))

def parseMethodCall(state, l, r):
	if type(r) != FnCallAST:
		r = FnCallAST(r, [], Span.merge(l.span, r.span))
	r.args.insert(0, l)
	return r

def parseIf(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	ifBlock = BlockAST(parseBlock(state, parseFnBodyExpr))
	span = Span.merge(span, ifBlock.span)
	
	offset = state.offset
	state.skipEmptyLines()
	if state.tok.type == TokenType.ELSE or \
		state.tok.type == TokenType.INDENT and \
		len(state.tok.content) == state.indentLevel and \
		state.tokens[state.offset+1].type == TokenType.ELSE:
		if state.tok.type == TokenType.INDENT:
			expectIndent(state)
		state.advance()
		state.skipSpace()
		
		if state.tok.type == TokenType.IF:
			tok = state.tok
			elseIf = parseIf(state)
			elseIfBlock = Block([elseIf], elseIf.span) if elseIf else Block([], tok.span)
			elseBlock = BlockAST(elseIfBlock)
		else:
			elseBlock = BlockAST(parseBlock(state, parseFnBodyExpr))
		
		span = Span.merge(span, elseBlock.span)
	else:
		state.rollback(offset)
		elseBlock = BlockAST(Block([], Span.cursor(state.tok.span)))
	
	return IfAST(expr, ifBlock, elseBlock, span)

def parseWhile(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	block = parseBlock(state, parseFnBodyExpr)
	span = Span.merge(span, block.span)
	
	return WhileAST(expr, BlockAST(block), span)

def parseLoop(state):
	span = state.tok.span
	state.advance()
	block = parseBlock(state, parseFnBodyExpr)
	span = Span.merge(span, block.span)
	
	return LoopAST(BlockAST(block), span)

def parseLoopCtl(state):
	span = state.tok.span
	isBreak = state.tok.type == TokenType.BREAK
	state.advance()
	# label = None
	# expr = None
	# state.skipSpace()
	
	# if state.tok.type == TokenType.COLON:
	# 	span = Span.merge(span, state.tok.span)
	# 	state.advance()
	# 	state.skipSpace()
	# 	if expectType(state, TokenType.NAME):
	# 		label = state.tok.content
	# 		span = Span.merge(span, state.tok.span)
	# 		state.advance()
	# 		state.skipSpace()
	
	# if state.tok.type in VALUE_EXPR_TOKS:
	# 	expr = parseValueExpr(state)
	# 	if expr:
	# 		span = Span.merge(span, expr.span)
	
	return BreakAST(span) if isBreak else ContinueAST(span)

VALUE_EXPR_TOKS = (
	TokenType.NEWLINE,
	TokenType.LBRACE,
	TokenType.LPAREN,
	TokenType.NAME,
	TokenType.STRING,
	TokenType.CHAR,
	TokenType.INTEGER,
	TokenType.FLOAT,
	TokenType.PLUS,
	TokenType.MINUS,
	TokenType.FALSE,
	TokenType.TRUE,
	TokenType.IF,
	TokenType.VOID
)

def parseDeref(state, expr):
	span = state.tok.span
	derefCount = 0
	
	while state.tok.type == TokenType.CARET:
		derefCount += 1
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
	
	return DerefAST(expr, derefCount, span)

def parseAddress(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseValueExpr(state)
	
	if type(expr) == AddressAST:
		logError(state, span, 'cannot take the address of an address')
	
	span = Span.merge(span, state.tok.span)
	return AddressAST(expr, span)

def parseIndex(state, expr):
	span = state.tok.span
	state.advance()
	
	index = parseValueExpr(state)
	permitLineBreak(state)
	if expectType(state, TokenType.RBRACK) == False:
		return None
	
	span = Span.merge(span, state.tok.span)
	state.advance()
	
	return IndexOpAST(expr, index, span)

def parseInfixOp(state, l, mustIndent, spaceAroundOp):
	op = state.tok.type
	opSpan = state.tok.span
	
	offset = state.offset
	state.advance()
	spaceAroundOp = state.skipSpace() or spaceAroundOp
	
	eol = False
	
	if op == TokenType.CARET:
		isUnary = False
		
		if state.tok.type == TokenType.NEWLINE:
			eol = True
			state.skipEmptyLines()
			if not isIndentIncrease(state):
				isUnary = True
		elif (not spaceAroundOp and state.tok.type == TokenType.LPAREN) \
			or state.tok.type not in VALUE_EXPR_TOKS:
			isUnary = True
		
		if isUnary:
			state.rollback(offset)
			# if op == TokenType.CARET:
			return parseDeref(state, l)
			# else:
			# 	assert 0
	else:
		eol = state.skipEmptyLines()
	
	if eol:
		if mustIndent or isIndentIncrease(state):
			expectIndentIncrease(state)
		else:
			expectIndent(state)
	
	r = parseValueExpr(state, INFIX_PRECEDENCE[op])
	if r == None:
		return l
	
	if op == TokenType.ARROW:
		return parseMethodCall(state, l, r)
	else:
		return InfixOpAST(l, r, InfixOp.fromTokenType(op), Span.merge(l.span, r.span), opSpan)

class Path:
	def __init__(self, path, span):
		self.path = path
		self.span = span

def parsePath(state):
	path = [state.tok]
	span = state.tok.span
	state.advance()
	onOneLine = True
	
	while state.tok.type == TokenType.PATH:
		span = Span.merge(span, state.tok.span)
		state.advance()
		
		if onOneLine:
			onOneLine = permitLineBreakIndent(state) == False
		else:
			permitLineBreak(state)
		
		if expectType(state, TokenType.NAME) == False:
			break
		
		path.append(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	if not onOneLine:
		state.popIndentLevel()
	
	return Path(path, span)

def parseStructLit(state, path):
	def parseFieldLit(state):
		if expectType(state, TokenType.NAME) == False:
			return None
		
		nameTok = state.tok
		span = state.tok.span
		
		state.advance()
		state.skipSpace()
		
		if expectType(state, TokenType.COLON) == False:
			return FieldDeclAST(nameTok, None, span)
		
		Span.merge(span, state.tok.span)
		
		state.advance()
		onOneLine = permitLineBreakIndent(state) == False
		state.skipSpace()
		
		expr = parseValueExpr(state)
		if expr:
			span = Span.merge(span, expr.span)
		
		if not onOneLine:
			state.popIndentLevel()
		
		return FieldLitAST(nameTok, expr, span)
	
	span = path.span
	block = parseBlock(state, parseFieldLit)
	span = Span.merge(span, block.span)
	return StructLitAST(path.path, block.list, span)

def parseSign(state):
	negate = state.tok.type == TokenType.MINUS
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = None
	
	if state.tok.type == TokenType.INTEGER:
		expr = IntLitAST(state.tok.content, negate, Span.merge(span, state.tok.span))
		state.advance()
	elif state.tok.type == TokenType.FLOAT:
		expr = FloatLitAST(state.tok.content, negate, Span.merge(span, state.tok.span))
		state.advance()
	else:
		expr = parseValueExpr(state, UNARY_PRECEDENCE)
		expr = SignAST(expr, negate, Span.merge(span, expr.span))
	
	return expr

def parseFieldAccess(state, expr):
	path = []
	span = expr.span
	onOneLine = True
	
	while state.tok.type == TokenType.DOT:
		span = Span.merge(span, state.tok.span)
		state.advance()
		
		if onOneLine:
			onOneLine = permitLineBreakIndent(state) == False
		else:
			permitLineBreak(state)
		
		if expectType(state, TokenType.NAME) == False:
			break
		
		path.append(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	if not onOneLine:
		state.popIndentLevel()
	
	return FieldAccessAST(expr, path, span)

def isStructLitStart(state):
	offset = state.offset
	state.skipSpace()
	result = False
	
	if state.tok.type == TokenType.LBRACE:
		state.advance()
		result = True
	elif state.skipEmptyLines() and isIndentIncrease(state):
		result = True
	
	if result == True:
		state.skip(TokenType.SPACE, TokenType.INDENT, TokenType.COMMENT, TokenType.NEWLINE)
		result = state.tok.type == TokenType.NAME and state.nextTok.type == TokenType.COLON
	
	state.rollback(offset)
	return result

def parseValueExpr(state, precedence=0):
	if state.tok.type == TokenType.NEWLINE:
		block = parseBlock(state, parseFnBodyExpr)
		expr = BlockAST(block)
	elif state.tok.type == TokenType.LBRACE:
		block = parseBlock(state, parseFnBodyExpr, requireBlockMarkers=True)
		expr = BlockAST(block)
	elif state.tok.type == TokenType.LPAREN:
		block = parseBlock(state, parseValueExpr, BlockMarkers.PAREN, True)
		if len(block.list) == 1 and not block.trailingSeparator:
			expr = block.list[0]
			expr.span = block.span
		else:
			expr = TupleLitAST(block.list, block.span)
	elif state.tok.type == TokenType.VOID:
		expr = VoidAST(state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.NAME:
		path = parsePath(state)
		if isStructLitStart(state):
			expr = parseStructLit(state, path)
		else:
			expr = ValueRefAST(path.path, path.span)
	elif state.tok.type == TokenType.STRING:
		expr = StrLitAST(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.CHAR:
		expr = CharLitAST(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.PLUS or state.tok.type == TokenType.MINUS:
		expr = parseSign(state)
	elif state.tok.type == TokenType.AMP:
		expr = parseAddress(state)
	elif state.tok.type == TokenType.INTEGER:
		expr = IntLitAST(state.tok.content, False, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.FLOAT:
		expr = FloatLitAST(state.tok.content, False, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.FALSE or state.tok.type == TokenType.TRUE:
		value = state.tok.type == TokenType.TRUE
		expr = BoolLitAST(value, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.IF:
		expr = parseIf(state)
	else:
		logError(state, state.tok.span, 'expected value expression, found {}'.format(state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE, TokenType.COMMA, TokenType.SEMICOLON, TokenType.RBRACE)
		expr = None
	
	if expr == None:
		return None
	
	indentsCount = len(state.indentLevels)
	
	while True:
		spaceBeforeOp = state.skipSpace()
		
		if state.tok.type == TokenType.LBRACK:
			expr = parseIndex(state, expr)
		elif state.tok.type == TokenType.LPAREN:
			expr = parseFnCall(state, expr)
		elif state.tok.type == TokenType.AS:
			expr = parseCoercion(state, expr)
		elif state.tok.type == TokenType.DOT:
			expr = parseFieldAccess(state, expr)
		elif state.tok.type in INFIX_PRECEDENCE and INFIX_PRECEDENCE[state.tok.type] > precedence:
			expr = parseInfixOp(state, expr, expr.span.startLine == expr.span.endLine, spaceBeforeOp)
		elif state.tok.type == TokenType.CARET:
			expr = parseDeref(state, expr)
		else:
			break
	
	while indentsCount < len(state.indentLevels):
		state.popIndentLevel()
	
	return expr

def parseValueExprOrAsgn(state):
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	state.skipSpace()
	if state.tok.type == TokenType.ASGN:
		if type(expr) not in (ValueRefAST, DerefAST, IndexOpAST, FieldAccessAST):
			logError(state, state.tok.span, "invalid assignment target in assignment")
		
		state.advance()
		state.skipSpace()
		
		lvalue = expr
		rvalue = parseValueExpr(state)
		
		expr = AsgnAST(lvalue, rvalue, Span.merge(lvalue.span, rvalue.span))
	
	return expr

def parseReturn(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	expr = None
	if state.tok.type not in (TokenType.COMMA, TokenType.SEMICOLON, \
		TokenType.RBRACE, TokenType.NEWLINE, TokenType.EOF):
		expr = parseValueExpr(state)
		if expr != None:
			span = Span.merge(span, expr.span)
	
	return ReturnAST(expr, span)

def parseLet(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	mut = False
	if state.tok.type == TokenType.MUT:
		mut = True
		state.advance()
		state.skipSpace()
	
	if expectType(state, TokenType.NAME) == False:
		return None
	
	name = state.tok.content
	span = Span.merge(span, state.tok.span)
	state.advance()
	state.skipSpace()
	
	typeRef = None
	if state.tok.type == TokenType.COLON:
		state.advance()
		state.skipSpace()
		typeRef = parseTypeRef(state)
		if typeRef != None:
			span = Span.merge(span, typeRef.span)
	
	state.skipSpace()
	expr = None
	if state.tok.type == TokenType.ASGN:
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
		expr = parseValueExpr(state)
		if expr != None:
			span = Span.merge(span, expr.span)
	
	if typeRef == None and expr == None:
		logError(state, span, '`let` binding requires either an explicit type or a value to assign')
	
	return LetAST(mut, name, typeRef, expr, span)

def parseFnBodyExpr(state):
	if state.tok.type in VALUE_EXPR_TOKS:
		return parseValueExprOrAsgn(state)
	elif state.tok.type == TokenType.RETURN:
		return parseReturn(state)
	elif state.tok.type == TokenType.LET:
		return parseLet(state)
	elif state.tok.type == TokenType.LOOP:
		return parseLoop(state)
	elif state.tok.type == TokenType.WHILE:
		return parseWhile(state)
	# elif state.tok.type == TokenType.FOR:
	# 	return parseFor(state)
	elif state.tok.type == TokenType.BREAK or state.tok.type == TokenType.CONTINUE:
		return parseLoopCtl(state)
	else:
		logError(state, state.tok.span, 'expected expression, found {}'.format(state.tok.type.desc()))

def parseFnDecl(state, doccomment, attrs, extern):
	span = state.tok.span
	startLine = state.tok.span.startLine
	onOneLine = True
	
	state.advance()
	state.skipSpace()
	
	nameTok = None
	if expectType(state, TokenType.NAME):
		nameTok = state.tok
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	params = []
	cVarArgs = False
	cVarArgsSpan = None
	if expectType(state, TokenType.LPAREN):
		block = parseFnDeclParams(state)
		params = block.list
		span = Span.merge(span, block.span)
		if len(params) > 0 and type(params[-1]) == CVarArgsParamAST:
			cVarArgsSpan = params.pop().span
			cVarArgs = True
	
	state.skipSpace()
	
	returnType = None
	if state.tok.type == TokenType.ARROW:
		returnType = parseFnDeclReturnType(state)
		if returnType != None:
			span = Span.merge(span, returnType.span)
	
	body = None
	if not extern:
		block = parseBlock(state, parseFnBodyExpr)
		body = BlockAST(block)
		span = Span.merge(span, body.span)
	
	return FnDeclAST(doccomment, attrs, extern, nameTok, params, cVarArgs, returnType, body, span, cVarArgsSpan)

def parseModLevelDecl(state):
	startToken = None
	doccomment = None
	if state.tok.type == TokenType.DOCCOMMENT:
		startToken = doccomment = state.tok
		state.advance()
		permitLineBreak(state)
	
	if state.tok.type == TokenType.MOD:
		if startToken == None:
			startToken = state.tok
		modDecl = parseModule(state, doccomment)
		if modDecl != None:
			modDecl.span = Span.merge(startToken.span, modDecl.span)
		return modDecl
	
	attrs = []
	if state.tok.type == TokenType.AT:
		if startToken == None:
			startToken = state.tok
		attrs = parseAttrs(state)
		permitLineBreak(state)
	
	extern = False
	if state.tok.type == TokenType.EXTERN:
		if startToken == None:
			startToken = state.tok
		extern = True
		state.advance()
		permitLineBreak(state)
	
	if startToken == None: startToken = state.tok
	
	if expectType(state, TokenType.FN, TokenType.LET, TokenType.STRUCT) == False:
		return None
	
	if state.tok.type == TokenType.FN:
		fnDecl = parseFnDecl(state, doccomment, attrs, extern)
		if fnDecl != None:
			fnDecl.span = Span.merge(startToken.span, fnDecl.span)
		return fnDecl
	elif state.tok.type == TokenType.STRUCT:
		if extern:
			logError(state, state.tok.span, '`struct`s cannot be declared `extern`')
		structDecl = parseStructDecl(state, doccomment, attrs, False)
		if structDecl != None:
			structDecl.span = Span.merge(startToken.span, structDecl.span)
		return structDecl
	else:
		logError(state, state.tok.span, 'static variables at the module level are unimplemented')
		return None

def parseModule(state, doccomment):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	nameTok = None
	if expectType(state, TokenType.NAME):
		nameTok = state.tok
		state.advance()
	
	block = parseBlock(state, parseModLevelDecl)
	modAST = ModAST(doccomment, nameTok, block.list, Span.merge(span, block.span))
	return modAST

def parseTopLevelModule(state):
	fileName = path.basename(state.source.fileName)
	name = re.match(r"^([^.]+)?", fileName)[1]
	
	if not re.match(r"^[a-zA-Z_][0-9a-zA-Z_]*$", name):
		logError(state, Span(state.source, 1, 1, 1, 1), 'illegal module name: `{}`'.format(name))
	
	block = parseBlock(state, parseModLevelDecl, topLevelBlock=True)
	modAST = ModAST(None, None, block.list, block.span, name)
	return modAST

def parse(source, tokens):
	state = ParserState(source, tokens)
	modAST = parseTopLevelModule(state)
	
	if state.failed:
		exit(1)
	
	return modAST