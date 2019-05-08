import re
from enum import Enum
from .token import TokenType, revealToken
from .span import Span, revealSpan, AnsiColor

#######################
#  Parser state
#######################

class ParserState:
	def __init__(self, source, tokens):
		self.source            = source
		self.tokens            = tokens
		self._offset           = 0
		self.tok               = self.tokens[0]
		self.eof               = self.tok.type == TokenType.EOF
		self.indentLevels      = [0]
		self.failed            = False
	
	def advance(self):
		if self.eof:
			return
		
		self._offset += 1
		self.tok = self.tokens[self._offset]
		self.eof = self.tok.type == TokenType.EOF
	
	@property
	def offset(self):
		return self._offset
	
	@property
	def indentLevel(self):
		return self.indentLevels[-1]
	
	def pushIndentLevel(self, level):
		return self.indentLevels.append(level)
	
	def popIndentLevel(self):
		if len(self.indentLevels) < 2:
			assert 0
		return self.indentLevels.pop()
	
	def rollback(self, offset):
		self._offset = offset
		self.tok = self.tokens[self._offset]
		self.eof = self.tok.type == TokenType.EOF
	
	def skip(self, *types):
		while self.tok.type in types:
			self.advance()
	
	def skipEmptyLines(self):
		while True:
			self.skip(TokenType.NEWLINE, TokenType.SPACE, TokenType.COMMENT)
			if self.tok.type == TokenType.INDENT:
				offset = self.offset
				self.skip(TokenType.INDENT, TokenType.SPACE, TokenType.COMMENT)
				if self.tok.type == TokenType.NEWLINE:
					continue
				else:
					self.rollback(offset)
			break
	
	def skipSpace(self):
		self.skip(TokenType.SPACE, TokenType.COMMENT)
	
	def skipAllSpace(self):
		self.skip(TokenType.SPACE, TokenType.INDENT, TokenType.NEWLINE, TokenType.COMMENT)
	
	def skipUntil(self, *types):
		while self.tok.type != TokenType.EOF and self.tok.type not in types:
			self.advance()


#######################
#  Operators & Types
#######################

# ->
# << >>
# * / %
# + -
# & ^ |
# ..< ...
# < <= > >= == !=
# &&
# ||

INFIX_OPS = {
	TokenType.ARROW:     900, 
	TokenType.LSHIFT:    800, 
	TokenType.RSHIFT:    800, 
	TokenType.TIMES:     700,
	TokenType.DIV:       700,
	TokenType.MODULO:    700, 
	TokenType.PLUS:      600, 
	TokenType.MINUS:     600,
	TokenType.BITAND:    500, 
	TokenType.BITOR:     500, 
	TokenType.CARET:     500, 
	TokenType.RNGCLOSED: 400, 
	TokenType.RNGOPEN:   400, 
	TokenType.EQ:        300, 
	TokenType.NEQ:       300, 
	TokenType.GREATER:   300, 
	TokenType.LESS:      300,
	TokenType.GREATEREQ: 300, 
	TokenType.LESSEQ:    300,
	TokenType.AND:       200, 
	TokenType.OR:        100
}

class InfixOp(Enum):
	ARROW = 'ARROW'
	LSHIFT = 'LSHIFT'
	RSHIFT = 'RSHIFT'
	TIMES = 'TIMES'
	DIV = 'DIV'
	MODULO = 'MODULO'
	PLUS = 'PLUS'
	MINUS = 'MINUS'
	BITAND = 'BITAND'
	BITOR = 'BITOR'
	BITXOR = 'BITXOR'
	RNGCLOSED = 'RNGCLOSED'
	RNGOPEN = 'RNGOPEN'
	EQ = 'EQ'
	NEQ = 'NEQ'
	GREATER = 'GREATER'
	LESS = 'LESS'
	GREATEREQ = 'GREATEREQ'
	LESSEQ = 'LESSEQ'
	AND = 'AND'
	OR = 'OR'
	
	@staticmethod
	def fromTokenType(type):
		if type == TokenType.ARROW:
			return InfixOp.ARROW
		elif type == TokenType.LSHIFT:
			return InfixOp.LSHIFT
		elif type == TokenType.RSHIFT:
			return InfixOp.RSHIFT
		elif type == TokenType.TIMES:
			return InfixOp.TIMES
		elif type == TokenType.DIV:
			return InfixOp.DIV
		elif type == TokenType.MODULO:
			return InfixOp.MODULO
		elif type == TokenType.PLUS:
			return InfixOp.PLUS
		elif type == TokenType.MINUS:
			return InfixOp.MINUS
		elif type == TokenType.BITAND:
			return InfixOp.BITAND
		elif type == TokenType.BITOR:
			return InfixOp.BITOR
		elif type == TokenType.CARET:
			return InfixOp.BITXOR
		elif type == TokenType.RNGCLOSED:
			return InfixOp.RNGCLOSED
		elif type == TokenType.RNGOPEN:
			return InfixOp.RNGOPEN
		elif type == TokenType.EQ:
			return InfixOp.EQ
		elif type == TokenType.NEQ:
			return InfixOp.NEQ
		elif type == TokenType.GREATER:
			return InfixOp.GREATER
		elif type == TokenType.LESS:
			return InfixOp.LESS
		elif type == TokenType.GREATEREQ:
			return InfixOp.GREATEREQ
		elif type == TokenType.LESSEQ:
			return InfixOp.LESSEQ
		elif type == TokenType.AND:
			return InfixOp.AND
		elif type == TokenType.OR:
			return InfixOp.OR
		else:
			return None

class Type:
	def __init__(self, name, byteSize):
		self.name = name
		self.byteSize = byteSize

PLATFORM_WORD_SIZE = 8
BUILTIN_TYPES = [
	Type('Bool',    1),
	Type('Int8',    1),
	Type('UInt8',   1),
	Type('Int16',   2),
	Type('UInt16',  2),
	Type('Int32',   4),
	Type('UInt32',  4),
	Type('Char',    4),
	Type('Int64',   8),
	Type('UInt64',  8),
	Type('ISize',   PLATFORM_WORD_SIZE),
	Type('USize',   PLATFORM_WORD_SIZE),
	Type('Float32', 4),
	Type('Float64', 8)
]


#######################
#  AST structs
#######################

class CConv(Enum):
	CAESAR = 'CAESAR'
	C = 'C'

def strBytes(s):
	result = ''
	esc = False
	for c in s[1:-1]:
		if c == '\\':
			esc = True
		elif esc:
			esc = False
			if c == 'n':
				result += '\n'
			elif c == 't':
				result += '\t'
			elif c == '"':
				result += c
			else:
				result += '\\' + c
		else:
			result += c
	
	return bytes(result, 'utf-8')

class ModAST:
	def __init__(self):
		self.symbolTable = {}
		self.span = None
		self.importDecls = []
		self.fnDecls = []
		self.staticDecls = []
		
		for type in BUILTIN_TYPES:
			self.symbolTable[type.name] = type
	
	def declImport(self, decl):
		self.symbolTable[decl.name] = decl
		self.importDecls.append(decl)
	
	def declStatic(self, decl):
		self.symbolTable[decl.name] = decl
		self.staticDecls.append(decl)
	
	def declFn(self, decl):
		self.symbolTable[decl.name] = decl
		self.fnDecls.append(decl)

class ModLevelDeclAST:
	def __init__(self, doccomment, attrs, extern, name, span):
		self.span = span
		self.name = name
		self.mangledName = name
		self.doccomment = doccomment
		self.attrs = attrs
		self.extern = extern

class AttrAST:
	def __init__(self, name, args, span):
		self.span = span
		self.name = name
		self.args = args

class FnDeclAST(ModLevelDeclAST):
	def __init__(self, doccomment, attrs, extern, name, params, cVarArgs, returnType, body, span):
		super().__init__(doccomment, attrs, extern, name, span)
		self.params = params
		self.cVarArgs = cVarArgs
		self.returnType = returnType
		self.body = body
		self.cconv = CConv.CAESAR

class FnParamAST:
	def __init__(self, name, type, span):
		self.name = name
		self.type = type
		self.span = span

class FnCallAST:
	def __init__(self, expr, args, span):
		self.expr = expr
		self.args = args
		self.span = span

class ReturnAST:
	def __init__(self, expr, span):
		self.expr = expr
		self.span = span

class LetAST:
	def __init__(self, mut, name, expr, span):
		self.mut = mut
		self.name = name
		self.expr = expr
		self.span = span

class TypeRefAST:
	def __init__(self, name, indirectionLevel, span):
		self.name = name
		self.indirectionLevel = indirectionLevel
		self.span = span

class ValueRefAST:
	def __init__(self, name, span):
		self.name = name
		self.span = span

class StrLitAST:
	def __init__(self, value, span):
		self.value = strBytes(value)
		self.span = span

class IntLitAST:
	def __init__(self, value, suffix, span):
		self.value = value
		self.suffix = suffix
		self.span = span

class TupleLitAST:
	def __init__(self, values, span):
		self.values = values
		self.span = span

class IfAST:
	def __init__(self, expr, ifBlock, elseBlock, span):
		self.expr = expr
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
		self.span = span

class InfixOpAST:
	def __init__(self, l, r, op, span):
		self.l = l
		self.r = r
		self.op = op
		self.span = span


#######################
#  Parser
#######################

def logError(state, token, message):
	state.failed = True
	print(revealToken(token, message))

def expectIndent(state):
	if state.tok.span.startColumn != 1:
		return True
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
		state.advance()
	
	level = 0 if indentTok == None else len(indentTok.content)
	if level != state.indentLevel:
		logError(state, state.tok, 'expected indent level {}, found level {}'
				.format(state.indentLevel, level))
		return False
	
	return True

def isIndentDecrease(state):
	if state.tok.span.startColumn != 1:
		return False
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
	
	level = 0 if indentTok == None else len(indentTok.content)
	return level < state.indentLevel

def expectIndentIncrease(state):
	if state.tok.span.startColumn != 1:
		return True
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
		state.advance()
	
	level = 0 if indentTok == None else len(indentTok.content)
	if level <= state.indentLevel:
		logError(state, state.tok, 'expected indented block')
		return False
	else:
		state.pushIndentLevel(level)
		return True

def expectIndentOrIndentIncrease(state):
	if state.tok.span.startColumn != 1:
		return True
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
		state.advance()
	
	level = 0 if indentTok == None else len(indentTok.content)
	if level < state.indentLevel:
		logError(state, state.tok, 'expected indented block')
		return False
	else:
		if level > state.indentLevel:
			state.pushIndentLevel(level)
		return True

def expectType(state, *types):
	if state.tok.type not in types:
		typesStr = ', '.join([type.desc() for type in types[0:-1]]) + \
			(',' if len(types) > 2 else '') + (' or ' if len(types) > 1 else '') + types[-1].desc()
		logError(state, state.tok, 'expected {}, found {}'
				.format(typesStr, state.tok.type.desc()))
		return False
	else:
		return True

def parseAttrArgs(state):
	def parseAttrArg(state):
		if state.tok.type == TokenType.STRING:
			arg = state.tok
			state.advance()
			return arg
		else:
			return None
	
	return parseBlock(state, parseAttrArg, TokenType.COMMA, BlockMarkers.PAREN, True)

def parseAttrs(state):
	attrs = []
	
	while state.tok.type == TokenType.AT:
		span = state.tok.span
		state.advance()
		
		if expectType(state, TokenType.NAME) == False:
			state.skipUntil(TokenType.SPACE, TokenType.COMMENT, TokenType.NEWLINE)
			state.skipSpace()
		else:
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
		
		state.skipEmptyLines()
	
	return attrs

def parseFnDeclParams(state):
	def parseFnParam(state):
		if state.tok.type == TokenType.ELLIPSIS:
			ret = state.tok
			state.advance()
			if state.tok.type != TokenType.RPAREN:
				logError(state, ret, 'C variadic parameter must come last')
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
		
		state.advance()
		state.skipSpace()
		
		type = parseTypeRef(state)
		
		return FnParamAST(name, type, Span.merge(span, state.tokens[state.offset-1].span))
	
	return parseBlock(state, parseFnParam, TokenType.COMMA, BlockMarkers.PAREN, True)

def parseFnDeclReturnType(state):
	span = state.tok.span
	state.advance() # skip `->`
	
	if state.tok.type == TokenType.NEWLINE:
		state.skipEmptyLines()
		expectIndentIncrease(state)
	
	state.skipSpace()
	return parseTypeRef(state)

class BlockMarkers(Enum):
	PAREN = 'PAREN'
	BRACE = 'BRACE'
	BRACK = 'BRACK'

class Block:
	def __init__(self, list, span, trailingSeparator):
		self.list = list
		self.span = span
		self.trailingSeparator = trailingSeparator

def parseBlock(state, parseItem, sepType=TokenType.SEMICOLON, 
	blockMarkers=BlockMarkers.BRACE, requireBlockMarkers=False):
	if blockMarkers == BlockMarkers.BRACE:
		openMarker = TokenType.LBRACE
		closeMarker = TokenType.RBRACE
	elif blockMarkers == BlockMarkers.BRACK:
		openMarker = TokenType.LBRACK
		closeMarker = TokenType.RBRACK
	else:
		openMarker = TokenType.LPAREN
		closeMarker = TokenType.RPAREN
	
	startLine = state.tok.span.startLine
	onOneLine = True
	
	offset = state.offset
	state.skipEmptyLines()
	
	if state.tok.type == TokenType.INDENT and state.tokens[state.offset+1].type == openMarker:
		expectIndent(state)
	
	startSpan = endSpan = state.tok.span
	
	needsTerm = False
	if state.tok.type == openMarker:
		startLine = state.tok.span.startLine
		needsTerm = True
		state.advance()
		state.skipEmptyLines()
	elif requireBlockMarkers:
		logError(state, state.tok, 'expected {}, found {}'.format(openMarker, state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE)
		return Block([], startSpan, False)
	
	if not needsTerm:
		state.rollback(offset)
		state.skipSpace()
		if expectType(state, TokenType.NEWLINE):
			state.skipEmptyLines()
	
	if onOneLine == True and startLine != state.tok.span.startLine:
		onOneLine = False
		if not needsTerm or state.tok.type != closeMarker:
			expectIndentIncrease(state)
	
	trailingSeparator = False
	list = []
	while True:
		if onOneLine or state.tok.type == TokenType.COMMENT:
			state.skipSpace()
		elif state.tok.type == TokenType.SPACE:
			logError(state, state.tok, 'expected expression, found space')
		
		if needsTerm and state.tok.type == closeMarker or state.tok.type == TokenType.EOF:
			if needsTerm: expectType(state, closeMarker)
			endSpan = state.tok.span
			state.advance()
			break
		
		trailingSeparator = False
		item = parseItem(state)
		if item == None:
			skipUntilTypes = (sepType, TokenType.NEWLINE, closeMarker) if needsTerm else (sepType, TokenType.NEWLINE)
			state.skipUntil(*skipUntilTypes)
		else:
			list.append(item)
			endSpan = item.span
		
		state.skipSpace()
		
		expectedTypes = (sepType, TokenType.NEWLINE, closeMarker) if needsTerm else (sepType, TokenType.NEWLINE)
		if expectType(state, *expectedTypes) == False:
			state.skipUntil(TokenType.NEWLINE)
		
		if state.tok.type == sepType:
			state.advance()
			trailingSeparator = True
		
		offset = state.offset
		state.skipEmptyLines()
		if onOneLine and startLine != state.tok.span.startLine and state.tok.type == TokenType.INDENT:
			expectIndentIncrease(state)
			onOneLine = False
		
		if not onOneLine:
			if needsTerm:
				offset = state.offset
				state.skip(TokenType.INDENT)
				if state.tok.type == closeMarker:
					endSpan = state.tok.span
					state.rollback(offset)
					state.popIndentLevel()
					expectIndent(state)
					state.advance()
					break
				
				state.rollback(offset)
				expectIndent(state)
			elif isIndentDecrease(state):
				state.popIndentLevel()
				state.rollback(offset)
				break
			else:
				expectIndent(state)
	
	return Block(list, Span.merge(startSpan, endSpan), trailingSeparator)

def parseTypeRef(state):
	if expectType(state, TokenType.NAME) == False:
		return None
	else:
		typeName = state.tok.content
		span = state.tok.span
		lastTok = state.tok
		state.advance()
		indirectionLevel = 0
		
		state.skipSpace()
		while state.tok.type == TokenType.CARET:
			lastTok = state.tok
			indirectionLevel += 1
			state.advance()
			state.skipSpace()
		
		return TypeRefAST(typeName, indirectionLevel, Span.merge(span, lastTok.span))

def parseFnCall(state, expr):
	block = parseBlock(state, parseValueExpr, TokenType.COMMA, BlockMarkers.PAREN, True)
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
	
	ifBlock = parseBlock(state, parseFnExpr)
	span = Span.merge(span, ifBlock.span)
	elseBlock = None
	
	state.skipEmptyLines()
	if state.tok.type == TokenType.ELSE or \
		state.tok.type == TokenType.INDENT and state.tokens[state.offset+1].type == TokenType.ELSE:
		expectIndent(state)
		state.advance()
		elseBlock = parseBlock(state, parseFnExpr)
		span = Span.merge(span, elseBlock.span)
	
	return IfAST(expr, ifBlock.list, elseBlock.list, span)

def parseInfixOp(state, l):
	op = state.tok.type
	
	state.advance()
	state.skipEmptyLines()
	expectIndentOrIndentIncrease(state)
	
	r = parseValueExpr(state, INFIX_OPS[op])
	if r == None:
		return l
	
	if op == TokenType.ARROW:
		return parseMethodCall(state, l, r)
	else:
		return InfixOpAST(l, r, InfixOp.fromTokenType(op), Span.merge(l.span, r.span))

def parseValueExpr(state, precedence=0):
	if state.tok.type == TokenType.LPAREN:
		block = parseBlock(state, parseValueExpr, TokenType.COMMA, BlockMarkers.PAREN, True)
		if len(block.list) == 1 and not block.trailingSeparator:
			expr = block.list[0]
			expr.span = block.span
		else:
			expr = TupleLitAST(block.list, block.span)
	elif state.tok.type == TokenType.NAME:
		expr = ValueRefAST(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.STRING:
		expr = StrLitAST(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.INTEGER:
		matches = re.match(r"(^[\d_]+)(:?i8|u8|i16|u16|i32|u32|i64|u64|sz|usz)?$", state.tok.content)
		value = matches[1].replace('_', '')
		suffix = matches[2]
		expr = IntLitAST(value, suffix, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.IF:
		expr = parseIf(state)
	else:
		logError(state, state.tok, 'expected value expression, found {}'.format(state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE, TokenType.SEMICOLON, TokenType.RBRACE)
		expr = None
	
	if expr == None:
		return None
	
	state.skipSpace()
	indentStack = len(state.indentLevels)
	
	while True:
		if state.tok.type == TokenType.LPAREN:
			expr = parseFnCall(state, expr)
		elif state.tok.type in INFIX_OPS and INFIX_OPS[state.tok.type] > precedence:
			expr = parseInfixOp(state, expr)
		else:
			break
	
	for _ in range(indentStack, len(state.indentLevels)):
		state.popIndentLevel()
	
	return expr

def parseReturn(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	expr = None
	if state.tok.type != TokenType.SEMICOLON and state.tok.type != TokenType.NEWLINE:
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
	state.advance()
	state.skipSpace()
	
	expr = None
	if state.tok.type == TokenType.ASGN:
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipEmptyLines()
		expectIndent(state)
		expr = parseValueExpr(state)
		if expr != None:
			span = Span.merge(span, expr.span)
	
	return LetAST(mut, name, expr, span)

def parseFnExpr(state):
	if state.tok.type in (TokenType.NAME, TokenType.STRING, TokenType.INTEGER, TokenType.IF):
		return parseValueExpr(state)
	elif state.tok.type == TokenType.RETURN:
		return parseReturn(state)
	elif state.tok.type == TokenType.LET:
		return parseLet(state)
	else:
		logError(state, state.tok, 'expected expression, found {}'.format(state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE, TokenType.SEMICOLON, TokenType.RBRACE)

def parseFnDecl(state, doccomment, attrs, extern):
	span = state.tok.span
	startLine = state.tok.span.startLine
	onOneLine = True
	state.advance() # skip `fn`
	
	state.skipSpace()
	if state.tok.type == TokenType.NEWLINE:
		state.skipEmptyLines()
		expectIndentIncrease(state)
	
	name = None
	if expectType(state, TokenType.NAME):
		name = state.tok.content
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	params = []
	cVarArgs = False
	if expectType(state, TokenType.LPAREN):
		block = parseFnDeclParams(state)
		params = block.list
		span = Span.merge(span, block.span)
		if len(params) > 0 and params[-1].type == TokenType.ELLIPSIS:
			params.pop()
			cVarArgs = True
	
	state.skipSpace()
	
	returnType = None
	if state.tok.type == TokenType.ARROW:
		returnType = parseFnDeclReturnType(state)
		if returnType != None:
			span = Span.merge(span, returnType.span)
	
	body = None
	if not extern:
		block = parseBlock(state, parseFnExpr)
		body = block.list
		span = Span.merge(span, block.span)
	
	return FnDeclAST(doccomment, attrs, extern, name, params, cVarArgs, returnType, body, span)

def parseModuleBody(state, modAST, braceTerm):
	
	while True:
		state.skipEmptyLines()
		if state.eof:
			break
		
		expectIndent(state)
		
		startToken = None
		doccomment = None
		if state.tok.type == TokenType.DOCCOMMENT:
			startToken = doccomment = state.tok
			state.advance()
			state.skipEmptyLines()
			expectIndent(state)
		
		attrs = []
		if state.tok.type == TokenType.AT:
			if startToken == None: startToken = state.tok
			attrs = parseAttrs(state)
		
		extern = False
		if state.tok.type == TokenType.EXTERN:
			if startToken == None: startToken = state.tok
			extern = True
			state.advance()
			state.skipAllSpace()
		
		if startToken == None: startToken = state.tok
		
		if expectType(state, TokenType.FN, TokenType.LET) == False:
			state.skipUntil(TokenType.NEWLINE, TokenType.SEMICOLON)
			state.advance()
			continue
		
		if state.tok.type == TokenType.FN:
			fnDecl = parseFnDecl(state, doccomment, attrs, extern)
			if fnDecl != None:
				fnDecl.span = Span.merge(startToken.span, fnDecl.span)
				modAST.declFn(fnDecl)
		else:
			state.skipUntil(TokenType.NEWLINE, TokenType.SEMICOLON)

def parse(source, tokens):
	state = ParserState(source, tokens)
	
	modAST = ModAST()
	modAST.span = Span(source, 1, 1, len(source.lines)+1, len(source.lines[-1])+1)
	parseModuleBody(state, modAST, False)
	
	if state.failed:
		exit(1)
	
	return modAST