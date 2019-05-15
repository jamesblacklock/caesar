import re
from os import path
from enum import Enum
from .token import TokenType, revealToken
from .span import Span, revealSpan, AnsiColor
from .types import BUILTIN_TYPES, InfixOp, INFIX_PRECEDENCE
from .err import logError

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
	
	return bytes(result, 'utf-8')

class ModLevelDeclAST:
	def __init__(self, doccomment, attrs, extern, nameTok, span):
		self.span = span
		self.nameTok = nameTok
		self.name = nameTok.content if nameTok else None
		self.mangledName = self.name
		self.doccomment = doccomment
		self.attrs = attrs
		self.extern = extern
		self.parentScope = None

class ModAST(ModLevelDeclAST):
	def __init__(self, state, doccomment, nameTok, decls, span, name=None):
		super().__init__(doccomment, None, False, nameTok, span)
		if name: self.name = name
		self.span = span
		self.importDecls = []
		self.fnDecls = []
		self.modDecls = []
		self.staticDecls = []
		self.symbolTable = {}
		
		for builtin in BUILTIN_TYPES:
			self.symbolTable[builtin.name] = builtin
		
		for decl in decls:
			if type(decl) == FnDeclAST:
				self.fnDecls.append(decl)
			elif type(decl) == ModAST:
				self.modDecls.append(decl)
			else:
				raise RuntimeError('unimplemented!')
			
			if decl.name in self.symbolTable:
				logError(state, decl.nameTok.span, 'cannot redeclare `{}` as a different symbol'.format(decl.name))
			else:
				self.symbolTable[decl.name] = decl

class AttrAST:
	def __init__(self, name, args, span):
		self.span = span
		self.name = name
		self.args = args

class FnDeclAST(ModLevelDeclAST):
	def __init__(self, doccomment, attrs, extern, nameTok, params, cVarArgs, returnType, body, span, cVarArgsSpan):
		super().__init__(doccomment, attrs, extern, nameTok, span)
		self.params = params
		self.cVarArgs = cVarArgs
		self.cVarArgsSpan = cVarArgsSpan
		self.returnType = returnType
		self.body = body
		self.cconv = CConv.CAESAR
		self.symbolTable = {}
		self.resolvedSymbolType = None

class FnParamAST:
	def __init__(self, name, typeRef, span):
		self.name = name
		self.typeRef = typeRef
		self.span = span
		self.resolvedSymbolType = None

class CVarArgsParamAST:
	def __init__(self, span):
		self.span = span

class ReturnAST:
	def __init__(self, expr, span):
		self.expr = expr
		self.span = span

class LetAST:
	def __init__(self, mut, name, typeRef, expr, span):
		self.mut = mut
		self.name = name
		self.typeRef = typeRef
		self.expr = expr
		self.span = span
		self.resolvedSymbolType = None

class TypeRefAST:
	def __init__(self, name, indirectionLevel, span):
		self.name = name
		self.path = [name]
		self.indirectionLevel = indirectionLevel
		self.span = span
		self.resolvedType = None

class AsgnAST:
	def __init__(self, lvalue, rvalue, span):
		self.lvalue = lvalue
		self.rvalue = rvalue
		self.span = span

class WhileAST:
	def __init__(self, expr, block, span):
		self.expr = expr
		self.block = block
		self.span = span
		self.parentScope = None
		self.symbolTable = {}

class ValueExprAST:
	def __init__(self):
		self.resolvedType = None

class StrLitAST(ValueExprAST):
	def __init__(self, value, span):
		super().__init__()
		self.value = strBytes(value)
		self.span = span

class BoolLitAST(ValueExprAST):
	def __init__(self, value, span):
		super().__init__()
		self.value = value
		self.span = span

class IntLitAST(ValueExprAST):
	def __init__(self, value, suffix, span):
		super().__init__()
		self.value = value
		self.suffix = suffix
		self.span = span

class TupleLitAST(ValueExprAST):
	def __init__(self, values, span):
		super().__init__()
		self.values = values
		self.span = span

class BlockAST(ValueExprAST):
	def __init__(self, exprs, span):
		super().__init__()
		self.exprs = exprs
		self.span = span
		self.parentScope = None
		self.symbolTable = {}

class ValueRefAST(ValueExprAST):
	def __init__(self, path, span):
		super().__init__()
		self.path = path
		self.name = path[-1]
		self.span = span

class InfixOpAST(ValueExprAST):
	def __init__(self, l, r, op, span, opSpan):
		super().__init__()
		self.l = l
		self.r = r
		self.op = op
		self.span = span
		self.opSpan = opSpan

class CoercionAST(ValueExprAST):
	def __init__(self, expr, typeRef, span):
		super().__init__()
		self.expr = expr
		self.typeRef = typeRef
		self.span = span

class FnCallAST(ValueExprAST):
	def __init__(self, expr, args, span):
		super().__init__()
		self.expr = expr
		self.args = args
		self.span = span

class IfAST(ValueExprAST):
	def __init__(self, expr, ifBlock, elseBlock, span):
		super().__init__()
		self.expr = expr
		self.ifBlock = ifBlock
		self.elseBlock = elseBlock
		self.span = span
		self.parentScope = None
		self.symbolTable = {}


#######################
#  Parser
#######################

def expectIndent(state):
	if state.tok.span.startColumn != 1:
		return True
	
	indentTok = None
	if state.tok.type == TokenType.INDENT:
		indentTok = state.tok
		state.advance()
	
	level = 0 if indentTok == None else len(indentTok.content)
	if level != state.indentLevel:
		logError(state, state.tok.span, 'expected indent level {}, found level {}'
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
		logError(state, state.tok.span, 'expected indented block')
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
		logError(state, state.tok.span, 'expected indented block')
		return False
	else:
		if level > state.indentLevel:
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
		
		state.advance()
		state.skipSpace()
		
		typeRef = parseTypeRef(state)
		
		return FnParamAST(name, typeRef, Span.merge(span, state.tokens[state.offset-1].span))
	
	return parseBlock(state, parseFnParam, TokenType.COMMA, BlockMarkers.PAREN, True)

def parseFnDeclReturnType(state):
	# span = state.tok.span
	state.advance() # skip `->`
	
	# if state.tok.type == TokenType.NEWLINE:
	# 	state.skipEmptyLines()
	# 	expectIndentIncrease(state)
	
	state.skipSpace()
	return parseTypeRef(state)

def parseCoercion(state, expr):
	span = Span.merge(expr.span, state.tok.span)
	state.advance() # skip `as`
	
	# if state.tok.type == TokenType.NEWLINE:
	# 	state.skipEmptyLines()
	# 	expectIndentIncrease(state)
	
	state.skipSpace()
	typeRef = parseTypeRef(state)
	if typeRef == None:
		return None
	
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

def parseBlock(state, parseItem, sepType=TokenType.COMMA, 
	blockMarkers=BlockMarkers.BRACE, requireBlockMarkers=False, topLevelBlock=False):
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
	onOneLine = not topLevelBlock
	
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
		logError(state, state.tok.span, 'expected {}, found {}'.format(openMarker, state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE)
		return Block([], startSpan, False)
	
	if not needsTerm and not topLevelBlock:
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
			logError(state, state.tok.span, 'expected expression, found space')
		
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
		
		if needsTerm:
			if expectType(state, TokenType.INDENT, sepType, TokenType.NEWLINE, closeMarker) == False:
				state.skipUntil(closeMarker)
		else:
			if expectType(state, TokenType.INDENT, sepType, TokenType.NEWLINE) == False:
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
			elif topLevelBlock and state.tok.type == TokenType.EOF:
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
	
	ifBlock = parseBlock(state, parseFnBodyExpr)
	span = Span.merge(span, ifBlock.span)
	elseBlock = None
	
	state.skipEmptyLines()
	if state.tok.type == TokenType.ELSE or \
		state.tok.type == TokenType.INDENT and state.tokens[state.offset+1].type == TokenType.ELSE:
		expectIndent(state)
		state.advance()
		state.skipSpace()
		
		if state.tok.type == TokenType.IF:
			tok = state.tok
			elseIf = parseIf(state)
			elseBlock = Block([elseIf], elseIf.span) if elseIf else Block([], tok.span)
		else:
			elseBlock = parseBlock(state, parseFnBodyExpr)
		
		span = Span.merge(span, elseBlock.span)
	
	return IfAST(expr, ifBlock.list, elseBlock.list if elseBlock else None, span)

def parseWhile(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	block = parseBlock(state, parseFnBodyExpr)
	span = Span.merge(span, block.span)
	
	return WhileAST(expr, block.list, span)

def parseInfixOp(state, l):
	op = state.tok.type
	opSpan = state.tok.span
	
	state.advance()
	state.skipEmptyLines()
	expectIndentOrIndentIncrease(state)
	
	r = parseValueExpr(state, INFIX_PRECEDENCE[op])
	if r == None:
		return l
	
	if op == TokenType.ARROW:
		return parseMethodCall(state, l, r)
	else:
		return InfixOpAST(l, r, InfixOp.fromTokenType(op), Span.merge(l.span, r.span), opSpan)

def parseValueRef(state):
	path = [state.tok.content]
	span = state.tok.span
	state.advance()
	
	while state.tok.type == TokenType.PATH:
		span = Span.merge(span, state.tok.span)
		state.advance()
		if expectType(state, TokenType.NAME) == False:
			break
		
		path.append(state.tok.content)
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	return ValueRefAST(path, span)

def parseValueExpr(state, precedence=0):
	state.skipSpace()
	if state.tok.type == TokenType.NEWLINE:
		block = parseBlock(state, parseFnBodyExpr)
		expr = BlockAST(block.list, block.span)
	elif state.tok.type == TokenType.LBRACE:
		block = parseBlock(state, parseFnBodyExpr, requireBlockMarkers=True)
		expr = BlockAST(block.list, block.span)
	elif state.tok.type == TokenType.LPAREN:
		block = parseBlock(state, parseValueExpr, TokenType.COMMA, BlockMarkers.PAREN, True)
		if len(block.list) == 1 and not block.trailingSeparator:
			expr = block.list[0]
			expr.span = block.span
		else:
			expr = TupleLitAST(block.list, block.span)
	elif state.tok.type == TokenType.NAME:
		expr = parseValueRef(state)
	elif state.tok.type == TokenType.STRING:
		expr = StrLitAST(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.INTEGER:
		matches = re.match(r"(^[\d_]+)(:?i8|u8|i16|u16|i32|u32|i64|u64|sz|usz)?$", state.tok.content)
		value = matches[1].replace('_', '')
		suffix = matches[2]
		expr = IntLitAST(value, suffix, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.FALSE or state.tok.type == TokenType.TRUE:
		value = state.tok.type == TokenType.TRUE
		expr = BoolLitAST(value, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.IF:
		expr = parseIf(state)
	else:
		logError(state, state.tok.span, 'expected value expression, found {}'.format(state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE, TokenType.COMMA, TokenType.RBRACE)
		expr = None
	
	if expr == None:
		return None
	
	indentStack = len(state.indentLevels)
	
	while True:
		state.skipSpace()
		
		if state.tok.type == TokenType.LPAREN:
			expr = parseFnCall(state, expr)
		elif state.tok.type == TokenType.AS:
			expr = parseCoercion(state, expr)
		elif state.tok.type in INFIX_PRECEDENCE and INFIX_PRECEDENCE[state.tok.type] > precedence:
			expr = parseInfixOp(state, expr)
		else:
			break
	
	for _ in range(indentStack, len(state.indentLevels)):
		state.popIndentLevel()
	
	return expr

def parseValueExprOrAsgn(state):
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	state.skipSpace()
	if state.tok.type == TokenType.ASGN:
		if type(expr) != ValueRefAST:
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
	if state.tok.type != TokenType.COMMA and state.tok.type != TokenType.NEWLINE:
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
	
	typeRef = None
	if state.tok.type == TokenType.COLON:
		state.advance()
		state.skipSpace()
		typeRef = parseTypeRef(state)
	
	expr = None
	if state.tok.type == TokenType.ASGN:
		span = Span.merge(span, state.tok.span)
		state.advance()
		expr = parseValueExpr(state)
		if expr != None:
			span = Span.merge(span, expr.span)
	
	return LetAST(mut, name, typeRef, expr, span)

def parseFnBodyExpr(state):
	if state.tok.type in (TokenType.NAME, TokenType.STRING, TokenType.INTEGER, TokenType.IF):
		return parseValueExprOrAsgn(state)
	elif state.tok.type == TokenType.RETURN:
		return parseReturn(state)
	elif state.tok.type == TokenType.LET:
		return parseLet(state)
	elif state.tok.type == TokenType.WHILE:
		return parseWhile(state)
	else:
		logError(state, state.tok.span, 'expected expression, found {}'.format(state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE, TokenType.COMMA, TokenType.RBRACE)

def parseFnDecl(state, doccomment, attrs, extern):
	span = state.tok.span
	startLine = state.tok.span.startLine
	onOneLine = True
	state.advance() # skip `fn`
	
	state.skipSpace()
	if state.tok.type == TokenType.NEWLINE:
		state.skipEmptyLines()
		expectIndentIncrease(state)
	
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
		body = block.list
		span = Span.merge(span, block.span)
	
	return FnDeclAST(doccomment, attrs, extern, nameTok, params, cVarArgs, returnType, body, span, cVarArgsSpan)

def parseModLevelDecl(state):
	startToken = None
	doccomment = None
	if state.tok.type == TokenType.DOCCOMMENT:
		startToken = doccomment = state.tok
		state.advance()
		state.skipEmptyLines()
		expectIndent(state)
	
	if state.tok.type == TokenType.MOD:
		if startToken == None: startToken = state.tok
		modDecl = parseModule(state, doccomment)
		if modDecl != None:
			modDecl.span = Span.merge(startToken.span, modDecl.span)
		
		return modDecl
	
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
		return None
	
	if state.tok.type == TokenType.FN:
		fnDecl = parseFnDecl(state, doccomment, attrs, extern)
		if fnDecl != None:
			fnDecl.span = Span.merge(startToken.span, fnDecl.span)
		return fnDecl
	else:
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
	return ModAST(state, doccomment, nameTok, block.list, Span.merge(span, block.span))

def parseTopLevelModule(state):
	span = Span(state.source, 1, 1, len(state.source.lines)+1, len(state.source.lines[-1])+1)
	fileName = path.basename(state.source.fileName)
	name = re.match(r"^([^.]+)?", fileName)[1]
	
	if not re.match(r"^[a-zA-Z_][0-9a-zA-Z_]*$", name):
		logError(state, Span(state.source, 1, 1, 1, 1), 'illegal module name: `{}`'.format(name))
	
	block = parseBlock(state, parseModLevelDecl, topLevelBlock=True)
	return ModAST(state, None, None, block.list, Span.merge(span, block.span), name=name)

def parse(source, tokens):
	state = ParserState(source, tokens)
	mod = parseTopLevelModule(state)
	
	if state.failed:
		exit(1)
	
	return mod