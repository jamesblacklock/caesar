import re
from os              import path
from enum            import Enum
from .token          import TokenType
from .span           import Span
from .log            import logError
from .infixops       import InfixOps, LOGIC_OPS
from .ast.sign       import Sign
from .ast.structdecl import FieldDecl, StructDecl, UnionFields
from .ast.tupledecl  import TupleDecl
from .ast.enumdecl   import EnumDecl, VariantDecl
from .ast.ast        import Attr, Name
from .ast.typeref    import PtrTypeRef, NamedTypeRef, ArrayTypeRef, OwnedTypeRef
from .symbol.mod     import Mod, Impl, TraitDecl
from .ast.fndecl     import FnDecl, CConv
from .ast.staticdecl import StaticDecl, ConstDecl
from .ast.asexpr     import AsExpr
from .ast.primitive  import IntLit, FloatLit, BoolLit, VoidLit
from .ast.strlit     import CharLit, StrLit
from .ast.block      import Block
from .ast.fncall     import FnCall
from .ast.ctlflow    import Return, Break, Continue
from .ast.loop       import While, Loop
from .ast.localdecl  import LetDecl, FnParam, CVarArgsParam
from .ast.asgn       import Asgn
from .ast.ifexpr     import If
from .ast.infix      import InfixOp
from .ast.logic      import LogicOp
from .ast.valueref   import ValueRef, Borrow
from .ast.field      import Index, Field
from .ast.deref      import Deref
from .ast.structlit  import StructLit, FieldLit
from .ast.tuplelit   import TupleLit, ArrayLit
from .ast.address    import Address
from .ast.typedecl   import TypeDecl
from .ast.isexpr     import Pattern, IsExpr
from .ast.importexpr import Import, ImportTree
from .ast.sizeof     import Sizeof, Offsetof
from .scope          import ScopeType

INFIX_PRECEDENCE = {
	TokenType.ARROW:  1000, TokenType.AS:        900, TokenType.IS:      900, TokenType.LSHIFT:    800, 
	TokenType.RSHIFT:  800, TokenType.TIMES:     700, TokenType.DIV:     700, TokenType.MODULO:    700, 
	TokenType.PLUS:    600, TokenType.MINUS:     600, TokenType.AMP:     500, TokenType.PIPE:      500, 
	TokenType.CARET:   500, TokenType.ELLIPSIS:  400, TokenType.RNGOPEN: 400, TokenType.EQ:        300, 
	TokenType.NEQ:     300, TokenType.GREATER:   300, TokenType.LESS:    300, TokenType.GREATEREQ: 300, 
	TokenType.LESSEQ:  300, TokenType.AND:       200, TokenType.OR:      100,
}

UNARY_PRECEDENCE = 9000

class BlockInfo:
	def __init__(self, list, span, trailingSeparator=False):
		self.list = list
		self.span = span
		self.trailingSeparator = trailingSeparator

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
		offset = self.offset
		while True:
			# skip to beginning of first non-empty line
			while self.tok.type in (TokenType.NEWLINE, TokenType.SPACE, TokenType.COMMENT):
				wasNewline = self.tok.type == TokenType.NEWLINE
				self.advance()
				if wasNewline:
					offset = self.offset
			self.rollback(offset)
			
			# if this is an indented but empty line, skip that, too
			if self.tok.type == TokenType.INDENT:
				self.skip(TokenType.INDENT, TokenType.SPACE, TokenType.COMMENT)
				if self.tok.type == TokenType.NEWLINE or self.tok.type == TokenType.EOF:
					self.advance()
					offset = self.offset
					continue
				else:
					self.rollback(offset)
			break
		
		if self.tok.span.startLine > line:
			return True
		else:
			self.skipSpace()
			return False
	
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

def checkIndent(state):
	if state.tok.span.startColumn != 1:
		return None
	
	if state.tok.type != TokenType.INDENT:
		return 0
	
	return len(state.tok.content)

def isIndentMatch(state):
	level = checkIndent(state)
	return level != None and level == state.indentLevel

def isIndentIncrease(state):
	level = checkIndent(state)
	return level != None and level > state.indentLevel

def isIndentDecrease(state):
	level = checkIndent(state)
	return level != None and level < state.indentLevel

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

def parseAttrArg(state):
	return parseExpr(state, ExprClass.ATTR)

def parseAttrs(state):
	attrs = []
	
	while state.tok.type == TokenType.AT:
		attr = parseAttr(state)
		attrs.append(attr)
		
		if state.skipEmptyLines():
			expectIndent(state)
	
	return attrs

def parseAttr(state, forParent=False):
	span = state.tok.span
	state.advance()	
	if expectType(state, TokenType.NAME) == False:
		return None
	
	span = Span.merge(span, state.tok.span)
	name = state.tok.content
	args = []
	state.advance()
	state.skipSpace()
	
	if not forParent and state.tok.type == TokenType.NEWLINE:
		state.skipEmptyLines()
		expectIndent(state)
	
	if state.tok.type == TokenType.LPAREN:
		block = parseBlock(state, parseAttrArg, BlockMarkers.PAREN, True)
		args = block.list
		span = Span.merge(span, block.span)
	
	return Attr(name, args, span)

def parseFnDeclParams(state):
	def parseFnParam(state):
		if state.tok.type == TokenType.ELLIPSIS:
			ret = CVarArgsParam(state.tok.span)
			state.advance()
			if state.tok.type != TokenType.RPAREN:
				logError(state, ret.span, 'C variadic parameter must come last')
				return None
			
			return ret
		
		mut = False
		if state.tok.type == TokenType.MUT:
			mut = True
			state.advance()
			state.skipSpace()
		
		if expectType(state, TokenType.NAME) == False:
			return None
		
		name = Name.fromTok(state.tok)
		span = state.tok.span
		
		state.advance()
		state.skipSpace()
		
		if expectType(state, TokenType.COLON) == False:
			return FnParam(name, None, mut, span)
		
		Span.merge(span, state.tok.span)
		
		state.advance()
		onOneLine = permitLineBreakIndent(state) == False
		state.skipSpace()
		
		typeRef = parseTypeRef(state)
		if typeRef:
			Span.merge(span, typeRef.span)
		
		if not onOneLine:
			state.popIndentLevel()
		
		return FnParam(name, typeRef, mut, span)
	
	return parseBlock(state, parseFnParam, BlockMarkers.PAREN, True)

def parseFnDeclReturnType(state):
	state.advance()
	onOneLine = permitLineBreakIndent(state) == False
	state.skipSpace()
	
	typeRef = parseTypeRef(state)
	
	if not onOneLine:
		state.popIndentLevel()
	
	return typeRef

def parseFieldDecl(state, isUnion):
	span = state.tok.span
	
	fieldAttrs = []
	if state.tok.type == TokenType.AT:
		fieldAttrs = parseAttrs(state)
		permitLineBreak(state)
	
	if not isUnion and state.tok.type == TokenType.UNION:
		state.advance()
		state.skipSpace()
		unionFields = parseBlock(state, lambda s: parseFieldDecl(s, True))
		return UnionFields(unionFields.list, Span.merge(span, unionFields.span))
	
	pub = False
	mut = False
	if state.tok.type == TokenType.PUB:
		pub = True
		state.advance()
		state.skipSpace()
		if state.tok.type == TokenType.MUT:
			mut = True
			state.advance()
			state.skipSpace()
	
	if expectType(state, TokenType.NAME) == False:
		return None
	
	name = Name.fromTok(state.tok)
	
	state.advance()
	state.skipSpace()
	
	if expectType(state, TokenType.COLON) == False:
		return FieldDecl(name, None, span)
	
	span = Span.merge(span, state.tok.span)
	
	state.advance()
	# onOneLine = permitLineBreakIndent(state) == False
	state.skipSpace()
	
	if isStructStart(state):
		typeRef = parseStructDecl(state, None, True)
	else:
		typeRef = parseTypeRef(state)
	
	if typeRef:
		span = Span.merge(span, typeRef.span)
	
	# if not onOneLine:
	# 	state.popIndentLevel()
	
	fieldDecl = FieldDecl(name, typeRef, pub, mut, span)
	fieldDecl.attrs = fieldAttrs
	return fieldDecl

def parseStructOrUnionDecl(state, doccomment, anon, pub, isUnion):
	span = state.tok.span
	if state.tok.type in (TokenType.STRUCT, TokenType.UNION):
		state.advance()
		state.skipSpace()
	
	name = None
	if not anon and expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
	
	block = parseBlock(state, lambda s: parseFieldDecl(s, isUnion))
	span = Span.merge(span, block.span)
	
	return StructDecl(name, isUnion, doccomment, block.list, pub, span)

def parseStructDecl(state, doccomment, anon, pub=False):
	return parseStructOrUnionDecl(state, doccomment, anon, pub, False)

def parseUnionDecl(state, doccomment, anon, pub=False):
	return parseStructOrUnionDecl(state, doccomment, anon, pub, True)

def parseEnumDecl(state, doccomment):
	def parseVariantDecl(state):
		span = state.tok.span
		name = None
		if expectType(state, TokenType.NAME):
			name = Name.fromTok(state.tok)
			state.advance()
			state.skipSpace()
		
		typeRef = None
		if state.tok.type == TokenType.LPAREN:
			typeRef = parseTupleDecl(state, None, True)
		elif isStructStart(state):
			typeRef = parseStructDecl(state, None, True)
		
		if typeRef:
			span = Span.merge(span, typeRef.span)
		return VariantDecl(name, typeRef, span)
	
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	name = None
	if expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
	
	block = parseBlock(state, parseVariantDecl)
	span = Span.merge(span, block.span)
	
	return EnumDecl(name, doccomment, block.list, span)

def parseAsExpr(state, expr):
	typeRef = parseTypeRef(state)
	if typeRef == None:
		return None
	
	return AsExpr(expr, typeRef, Span.merge(expr.span, typeRef.span))

def parseName(state):
	if expectType(state, TokenType.NAME):
		tok = state.tok
		state.advance()
		state.skipSpace()
		return tok

def parsePattern(state):
	# if state.tok.type == TokenType.OWNED:
	# 	return parseOwnedTypeRef(state)
	# elif state.tok.type in (TokenType.AMP, TokenType.AND):
	# 	return parsePtrTypeRef(state)
	# elif state.tok.type == TokenType.LBRACK:
	# 	return parseArrayTypeRef(state)
	# elif state.tok.type == TokenType.LPAREN:
	# 	return parseTupleDecl(state)
	# elif state.tok.type in (TokenType.LBRACE, TokenType.STRUCT):
	# 	return parseStructDecl(state, None, True)
	# elif state.tok.type == TokenType.UNION:
	# 	return parseUnionDecl(state, None, True)
	# el
	if expectType(state, TokenType.NAME, TokenType.VOID):
		path = parsePath(state)
		span = path.span
		path = path.path
		names = None
		if state.tok.type == TokenType.LPAREN:
			names = parseBlock(state, parseName, BlockMarkers.PAREN, True)
			span = Span.merge(span, names.span)
			names = names.list
		return Pattern(path, names, span)
	else:
		return None

def parseIsExpr(state, expr):
	pattern = parsePattern(state)
	if pattern == None:
		return None
	
	return IsExpr(expr, pattern, Span.merge(expr.span, pattern.span))

class BlockMarkers(Enum):
	PAREN = 'PAREN'
	BRACE = 'BRACE'
	BRACK = 'BRACK'

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
				state.tok.type == TokenType.INDENT and state.nextTok.type == openMarker and not isIndentIncrease(state):
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
			return BlockInfo([], startSpan, False)
	
	# if block marker is required, we should have found it by now
	if requireBlockMarkers and not needsTerm:
		assert expectType(state, openMarker) == False
		state.skipUntil(TokenType.NEWLINE)
		return BlockInfo([], startSpan, False)
	
	
	trailingSeparator = False
	itemList = []
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
		if not isBlockStart(state) and  state.skipEmptyLines():
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
			# skipUntilTypes = (TokenType.COMMA, TokenType.SEMICOLON, TokenType.NEWLINE, closeMarker) \
			# 	if needsTerm else (TokenType.COMMA, TokenType.SEMICOLON, TokenType.NEWLINE)
			skipUntilTypes = (TokenType.NEWLINE, closeMarker) if needsTerm else (TokenType.NEWLINE,)
			state.skipUntil(*skipUntilTypes)
		else:
			if type(item) == BlockInfo:
				itemList.extend(item.list)
			else:
				itemList.append(item)
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
	
	return BlockInfo(itemList, Span.merge(startSpan, endSpan), trailingSeparator)

def parsePtrTypeRef(state):
	span = state.tok.span
	indLevel = 0
	while state.tok.type in (TokenType.AMP, TokenType.AND):
		span = Span.merge(span, state.tok.span)
		indLevel += 1 if state.tok.type == TokenType.AMP else 2
		state.advance()
		state.skipSpace()
	
	mut = False
	if state.tok.type == TokenType.MUT:
		mut = True
		state.advance()
		state.skipSpace()
	
	baseType = parseTypeRef(state)
	if baseType == None:
		return None
	
	return PtrTypeRef(baseType, indLevel, mut, Span.merge(span, baseType.span))

def parseTupleDecl(state, doccomment, anon, pub=False):
	span = state.tok.span
	if state.tok.type == TokenType.TUPLE:
		state.advance()
		state.skipSpace()
	
	name = None
	if not anon and expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
	
	block = parseBlock(state, parseTypeRef, BlockMarkers.PAREN, True)
	span = Span.merge(span, block.span)
	
	return TupleDecl(name, doccomment, block.list, pub, span)

def parseArrayTypeRef(state):
	baseType = None
	count = None
	span = state.tok.span
	
	state.advance()
	state.skipSpace()
	baseType = parseTypeRef(state)
	state.skipSpace()
	if baseType == None or expectType(state, TokenType.TIMES) == False:
		return None
	state.advance()
	state.skipSpace()
	if expectType(state, TokenType.INTEGER) == False:
		return None
	count = int(state.tok.content)
	state.advance()
	state.skipSpace()
	if expectType(state, TokenType.RBRACK) == False:
		return None
		
	span = Span.merge(span, state.tok.span)
	state.advance()
	return ArrayTypeRef(baseType, count, span)

def parseOwnedTypeRef(state):
	def parseItem(state):
		if expectType(state, TokenType.NAME):
			path = parsePath(state)
			return ValueRef(path.path, path.span)
		else:
			return None
	
	span = state.tok.span
	
	state.advance()
	state.skipSpace()
	
	isValid = True
	acquire = None
	release = None
	if state.tok.type == TokenType.LPAREN:
		isValid = False
		ownedParams = parseBlock(state, parseItem, BlockMarkers.PAREN, True)
		if len(ownedParams.list) == 2:
			isValid = True
			acquire = ownedParams.list[0]
			release = ownedParams.list[1]
		else:
			logError(state, ownedParams.span, 
				'expected 2 params for (acquire, release) (found {})'.format(len(ownedParams.list)))
		state.skipSpace()
	
	typeRef = parseTypeRef(state)
	if typeRef and isValid:
		typeRef = OwnedTypeRef(typeRef, acquire, release, Span.merge(span, typeRef.span))
	
	return typeRef

def parseTypeRef(state):
	if state.tok.type == TokenType.OWNED:
		return parseOwnedTypeRef(state)
	elif state.tok.type in (TokenType.AMP, TokenType.AND):
		return parsePtrTypeRef(state)
	elif state.tok.type == TokenType.LBRACK:
		return parseArrayTypeRef(state)
	elif state.tok.type in (TokenType.LPAREN, TokenType.TUPLE):
		return parseTupleDecl(state, None, True)
	elif state.tok.type in (TokenType.LBRACE, TokenType.STRUCT):
		return parseStructDecl(state, None, True)
	elif state.tok.type == TokenType.UNION:
		return parseUnionDecl(state, None, True)
	elif expectType(state, TokenType.NAME, TokenType.VOID):
		path = parsePath(state)
		return NamedTypeRef(path.path, path.span)
	else:
		return None

def parseSimpleFnCall(state, expr):
	span = expr.span
	args = []
	while state.tok.type in VALUE_EXPR_TOKS and state.tok.type != TokenType.NEWLINE:
		arg = parseValueExpr(state, 0, True)
		if arg == None:
			state.skipSpace()
			continue
		args.append(arg)
		span = Span.merge(span, arg.span)
		
		if state.tok.type in (TokenType.NEWLINE, TokenType.COMMA, TokenType.SEMICOLON):
			break
		
		if state.skipSpace() == False:
			logError(state, state.tok.span, 'expected space, found {}'.format(state.tok.type.desc()))
	
	return FnCall(expr, args, span)

def parseFnCall(state, expr):
	block = parseBlock(state, parseValueExpr, BlockMarkers.PAREN, True)
	return FnCall(expr, block.list, Span.merge(expr.span, block.span))

def parseMethodCall(state, l, r):
	if type(r) != FnCall:
		if type(r) != ValueRef or len(r.path) != 1:
			logError(state, r.span, 'found unexpected expression instead of method call')
		r = FnCall(r, [], r.span)
	r.args.insert(0, l)
	r.isMethodCall = True
	r.span = Span.merge(l.span, r.span)
	return r

def parseIf(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	ifBlock = Block.fromInfo(parseBlock(state, parseFnBodyExpr), ScopeType.IF)
	span = Span.merge(span, ifBlock.span)
	
	offset = state.offset
	state.skipEmptyLines()
	elseBlock = None
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
			exprs = [elseIf] if elseIf else []
			span = elseIf.span if elseIf else tok.span
			elseBlock = Block(exprs, ScopeType.ELSE, span)
		else:
			elseBlock = Block.fromInfo(parseBlock(state, parseFnBodyExpr), ScopeType.ELSE)
		
		span = Span.merge(span, elseBlock.span)
	else:
		elseBlock = Block([], ScopeType.ELSE, span.endSpan())
		state.rollback(offset)
	
	return If(expr, ifBlock, elseBlock, span)

def parseWhile(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseValueExpr(state)
	if expr == None:
		return None
	
	blockInfo = parseBlock(state, parseFnBodyExpr)
	span = Span.merge(span, blockInfo.span)
	
	return While(expr, Block.fromInfo(blockInfo, ScopeType.LOOP), span)

def parseLoop(state):
	span = state.tok.span
	state.advance()
	blockInfo = parseBlock(state, parseFnBodyExpr)
	span = Span.merge(span, blockInfo.span)
	
	return Loop(Block.fromInfo(blockInfo, ScopeType.LOOP), span)

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
	
	return Break(span) if isBreak else Continue(span)

def parseDeref(state, expr):
	count = 0
	span = expr.span
	while state.tok.type == TokenType.CARET:
		count += 1
		span = Span.merge(span, state.tok.span)
		state.advance()
		state.skipSpace()
	
	if type(expr) == Deref:
		expr.count += count
	else:
		expr = Deref(expr, count, span)
	
	return expr

def parseAddress(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	mut = False
	if state.tok.type == TokenType.MUT:
		mut = True
		state.advance()
		state.skipSpace()
	
	expr = parseValueExpr(state, UNARY_PRECEDENCE)
	if expr == None:
		return None
	
	if type(expr) == Address:
		logError(state, span, 'cannot take the address of an address')
	
	span = Span.merge(span, expr.span)
	return Address(expr, mut, span)

def parseIndex(state, expr):
	state.advance()
	
	index = parseValueExpr(state)
	permitLineBreak(state)
	if expectType(state, TokenType.RBRACK) == False:
		return None
	
	span = Span.merge(expr.span, state.tok.span)
	state.advance()
	
	return Index(expr, index, span)

def parseInfixOps(state, l, mustIndent, spaceAroundOp):
	opTok = state.tok
	
	offset = state.offset
	state.advance()
	
	errSpaceTok = None
	if not spaceAroundOp and state.tok.type in (TokenType.COMMENT, TokenType.SPACE):
		errSpaceTok = state.tok
	
	state.skipSpace()
	
	eol = False
	
	if opTok.type == TokenType.CARET:
		isUnary = False
		
		if state.tok.type == TokenType.NEWLINE:
			eol = True
			state.skipEmptyLines()
			if not isIndentIncrease(state):
				isUnary = True
		elif (not spaceAroundOp and state.tok.type in (TokenType.LPAREN, TokenType.LBRACK)) \
			or state.tok.type not in VALUE_EXPR_TOKS:
			isUnary = True
		
		if isUnary:
			state.rollback(offset)
			return parseDeref(state, l)
		elif errSpaceTok:
			logError(state, errSpaceTok.span, 
				'expected value expression, found {}'.format(errSpaceTok.type.desc()))
	else:
		offset = state.offset
		eol = state.skipEmptyLines()
	
	if eol:
		didIndent = False
		if mustIndent or isIndentIncrease(state):
			didIndent = expectIndentIncrease(state)
		else:
			didIndent = expectIndent(state)
		
		if not didIndent:
			state.rollback(offset)
			return l
	
	if opTok.type == TokenType.AS:
		return parseAsExpr(state, l)
	elif opTok.type == TokenType.IS:
		return parseIsExpr(state, l)
	
	r = parseValueExpr(state, INFIX_PRECEDENCE[opTok.type])
	if r == None:
		return l
	
	if opTok.type == TokenType.ARROW:
		return parseMethodCall(state, l, r)
	else:
		span = Span.merge(l.span, r.span)
		op = opTok.toInfixOp()
		if op in LOGIC_OPS:
			return LogicOp(l, r, op, opTok.span, span)
		else:
			return InfixOp(l, r, op, opTok.span, span)

class Path:
	def __init__(self, path, span):
		self.path = path
		self.span = span

def parsePath(state, allowTrailingPath=False):
	path = [Name.fromTok(state.tok)]
	span = state.tok.span
	state.advance()
	onOneLine = True
	
	while state.tok.type == TokenType.PATH:
		if allowTrailingPath and state.nextTok.type != TokenType.NAME:
			break
		
		span = Span.merge(span, state.tok.span)
		state.advance()
		
		if onOneLine:
			onOneLine = permitLineBreakIndent(state) == False
		else:
			permitLineBreak(state)
		
		if expectType(state, TokenType.NAME) == False:
			break
		
		path.append(Name.fromTok(state.tok))
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	if not onOneLine:
		state.popIndentLevel()
	
	return Path(path, span)

def parseStructLit(state, typeRef):
	def parseFieldLit(state):
		if expectType(state, TokenType.NAME) == False:
			return None
		
		name = Name.fromTok(state.tok)
		span = state.tok.span
		
		state.advance()
		state.skipSpace()
		
		if expectType(state, TokenType.COLON) == False:
			return FieldDecl(name, None, span)
		
		Span.merge(span, state.tok.span)
		
		state.advance()
		onOneLine = permitLineBreakIndent(state) == False
		state.skipSpace()
		
		expr = parseValueExpr(state)
		if expr:
			span = Span.merge(span, expr.span)
		
		if not onOneLine:
			state.popIndentLevel()
		
		return FieldLit(name, expr, span)
	
	isUnion = False
	if typeRef:
		span = typeRef.span
	else:
		span = state.tok.span
		if state.tok.type in (TokenType.STRUCT, TokenType.UNION):
			isUnion = state.tok.type == TokenType.UNION
			state.advance()
			state.skipSpace()
	
	block = parseBlock(state, parseFieldLit)
	span = Span.merge(span, block.span)
	return StructLit(typeRef, isUnion, block.list, span)

def parseOffsetof(state):
	class ItemParser:
		def __init__(self):
			self.failed = False
			self.first = True
		
		def __call__(self, state):
			if self.first:
				self.first = False
				result = parseTypeRef(state)
			elif expectType(state, TokenType.NAME, TokenType.INTEGER):
				result = state.tok
				state.advance()
			
			if result == None:
				self.failed
			
			return result
	
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	first = True
	itemParser = ItemParser()
	path = parseBlock(state, itemParser, BlockMarkers.PAREN, True)
	
	if not itemParser.failed and len(path.list) < 2:
		logError(state, path.span, 
			'offsetof requires a minimum of 2 parameters (found {})'.format(len(path.list)))
		return None
	
	return Offsetof(path.list[0], path.list[1:], path.span)
	
def parseSign(state):
	negate = state.tok.type == TokenType.MINUS
	span = state.tok.span
	state.advance()
	expr = None
	
	if state.tok.type == TokenType.INTEGER:
		expr = IntLit(state.tok.content, negate, Span.merge(span, state.tok.span))
		state.advance()
	elif state.tok.type == TokenType.FLOAT:
		expr = FloatLit(state.tok.content, negate, Span.merge(span, state.tok.span))
		state.advance()
	else:
		expr = parseValueExpr(state, UNARY_PRECEDENCE)
		if expr == None:
			return None
		expr = Sign(expr, negate, Span.merge(span, expr.span))
	
	return expr

def parseField(state, expr):
	path = []
	span = expr.span
	onOneLine = True
	derefSpan = None
	deref = state.tok.type == TokenType.DEREFDOT
	if deref:
		derefSpan = span.clone()
		derefSpan.endColumn += 1
	
	while True:
		span = Span.merge(span, state.tok.span)
		state.advance()
		
		if onOneLine:
			onOneLine = permitLineBreakIndent(state) == False
		else:
			permitLineBreak(state)
		
		if expectType(state, TokenType.NAME, TokenType.INTEGER) == False:
			break
		
		path.append(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
		
		if state.tok.type != TokenType.DOT:
			break
	
	if not onOneLine:
		state.popIndentLevel()
	
	if deref:
		expr = Deref(expr, 1, derefSpan)
	
	return Field(expr, path, span)

def isBlockStart(state):
	offset = state.offset
	state.skipSpace()
	result = False
	expectBrace = False
	
	if state.tok.type == TokenType.LBRACE:
		result = True
	elif state.skipEmptyLines():
		if isIndentIncrease(state):
			result = True
		elif isIndentMatch(state):
			if state.tok.type == TokenType.INDENT:
				state.advance()
			state.skipSpace()
			result = state.tok.type == TokenType.LBRACE
	
	state.rollback(offset)
	return result

def isStructStart(state):
	offset = state.offset
	state.skipSpace()
	result = False
	expectBrace = False
	
	if state.tok.type == TokenType.LBRACE:
		state.advance()
		result = True
		expectBrace = True
	elif state.skipEmptyLines() and isIndentIncrease(state):
		result = True
	
	if result == True:
		state.skip(TokenType.SPACE, TokenType.INDENT, TokenType.COMMENT, TokenType.NEWLINE)
		result = state.tok.type == TokenType.NAME and state.nextTok.type == TokenType.COLON
		result = result or expectBrace and state.tok.type == TokenType.RBRACE
	
	state.rollback(offset)
	return result

def parseUnsafeBlock(state):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	expr = parseExpr(state, ExprClass.FN, allowSimpleFnCall=True)
	if type(expr) != Block:
		expr = Block([expr], ScopeType.BLOCK, expr.span)
	
	expr.span = Span.merge(span, expr.span)
	expr.unsafe = True
	return expr

def parseBlockOrAnonStructLit(state):
	if isStructStart(state):
		return parseStructLit(state, None)
	
	block = parseBlock(state, parseFnBodyExpr)
	if len(block.list) == 1 and block.list[0].hasValue:
		return block.list[0]
	else:
		return Block.fromInfo(block, ScopeType.BLOCK)

def parseValueExpr(state, precedence=0, noSkipSpace=False, allowSimpleFnCall=False):
	return parseExpr(state, ExprClass.VALUE_EXPR, precedence, noSkipSpace, allowSimpleFnCall)

def parseValueExprImpl(state, precedence, noSkipSpace, allowSimpleFnCall):
	if state.tok.type == TokenType.UNSAFE:
		expr = parseUnsafeBlock(state)
	elif state.tok.type == TokenType.NEWLINE:
		expr = parseBlockOrAnonStructLit(state)
	elif state.tok.type == TokenType.LBRACK:
		block = parseBlock(state, parseValueExpr, requireBlockMarkers=True, blockMarkers=BlockMarkers.BRACK)
		expr = ArrayLit(block.list, block.span)
	elif state.tok.type == TokenType.LBRACE:
		expr = parseBlockOrAnonStructLit(state)
	elif state.tok.type == TokenType.LPAREN:
		block = parseBlock(state, parseValueExpr, BlockMarkers.PAREN, True)
		if len(block.list) == 1 and not block.trailingSeparator:
			expr = block.list[0]
			expr.span = block.span
		else:
			expr = TupleLit(block.list, block.span)
	elif state.tok.type == TokenType.VOID:
		expr = VoidLit(state.tok.span)
		state.advance()
	elif state.tok.type in (TokenType.STRUCT, TokenType.UNION):
		state.advance()
		state.skipSpace()
		expr = parseStructLit(state, None)
	elif state.tok.type == TokenType.TUPLE:
		state.advance()
		state.skipSpace()
		block = parseBlock(state, parseValueExpr, BlockMarkers.PAREN, True)
		expr = TupleLit(block.list, block.span)
	elif state.tok.type == TokenType.NAME:
		path = parsePath(state)
		if isStructStart(state):
			typeRef = NamedTypeRef(path.path, path.span)
			expr = parseStructLit(state, typeRef)
		else:
			expr = ValueRef(path.path, path.span)
	elif state.tok.type == TokenType.STRING:
		expr = StrLit(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.CHAR:
		expr = CharLit(state.tok.content, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.PLUS or state.tok.type == TokenType.MINUS:
		expr = parseSign(state)
	elif state.tok.type == TokenType.BORROW:
		span = state.tok.span
		state.advance()
		state.skipSpace()
		expr = parseValueExpr(state, precedence=UNARY_PRECEDENCE)
		if expr:
			expr = Borrow(expr, Span.merge(span, expr.span))
	elif state.tok.type == TokenType.SIZEOF:
		span = state.tok.span
		state.advance()
		state.skipSpace()
		expr = parseTypeRef(state)
		if expr:
			expr = Sizeof(expr, Span.merge(span, expr.span))
	elif state.tok.type == TokenType.OFFSETOF:
		expr = parseOffsetof(state)
	elif state.tok.type == TokenType.AMP:
		expr = parseAddress(state)
	elif state.tok.type == TokenType.INTEGER:
		expr = IntLit(state.tok.content, False, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.FLOAT:
		expr = FloatLit(state.tok.content, False, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.FALSE or state.tok.type == TokenType.TRUE:
		value = state.tok.type == TokenType.TRUE
		expr = BoolLit(value, state.tok.span)
		state.advance()
	elif state.tok.type == TokenType.IF:
		expr = parseIf(state)
	else:
		logError(state, state.tok.span, 'expected value expression, found {}'.format(state.tok.type.desc()))
		state.skipUntil(TokenType.NEWLINE, TokenType.COMMA, TokenType.SEMICOLON, 
			TokenType.RPAREN, TokenType.RBRACE, TokenType.RBRACK)
		expr = None
	
	if expr == None:
		return None
	
	indentsCount = len(state.indentLevels)
	
	while True:
		if state.tok.type == TokenType.LBRACK:
			expr = parseIndex(state, expr)
		elif state.tok.type == TokenType.LPAREN:
			expr = parseFnCall(state, expr)
		elif state.tok.type in (TokenType.DOT, TokenType.DEREFDOT):
			expr = parseField(state, expr)
		else:
			offset = state.offset
			spaceBeforeOp = False if noSkipSpace else state.skipSpace()
			if state.tok.type in INFIX_PRECEDENCE and INFIX_PRECEDENCE[state.tok.type] > precedence:
				expr = parseInfixOps(state, expr, expr.span.startLine == expr.span.endLine, spaceBeforeOp)
			elif state.tok.type == TokenType.CARET:
				expr = parseDeref(state, expr)
			elif allowSimpleFnCall and type(expr) == ValueRef and state.tok.type != TokenType.NEWLINE and \
				state.tok.type in VALUE_EXPR_TOKS:
				expr = parseSimpleFnCall(state, expr)
			else:
				spaceBeforeOp = state.skipSpace()
				if state.tok.type == TokenType.AS and INFIX_PRECEDENCE[state.tok.type] > precedence:
					expr = parseInfixOps(state, expr, expr.span.startLine == expr.span.endLine, spaceBeforeOp)
				else:
					state.rollback(offset)
					break
	
	while indentsCount < len(state.indentLevels):
		state.popIndentLevel()
	
	return expr

def parseValueExprOrAsgn(state):
	expr = parseValueExpr(state, allowSimpleFnCall=True)
	if expr == None:
		return None
	
	state.skipSpace()
	if state.tok.type in ASGN_OPER_TOKS:
		opTok = state.tok
		infixOp = None
		if opTok.type != TokenType.ASGN:
			if opTok.type == TokenType.TIMESASGN:
				infixOp = InfixOps.TIMES
			elif opTok.type == TokenType.DIVASGN:
				infixOp = InfixOps.DIV
			elif opTok.type == TokenType.MODULOASGN:
				infixOp = InfixOps.MODULO
			elif opTok.type == TokenType.PLUSASGN:
				infixOp = InfixOps.PLUS
			elif opTok.type == TokenType.MINUSASGN:
				infixOp = InfixOps.MINUS
			elif opTok.type == TokenType.ANDASGN:
				infixOp = InfixOps.BITAND
			elif opTok.type == TokenType.ORASGN:
				infixOp = InfixOps.BITOR
			elif opTok.type == TokenType.XORASGN:
				infixOp = InfixOps.BITXOR
			else:
				assert 0
		
		if type(expr) not in (ValueRef, Index, Deref, Field):
			logError(state, state.tok.span, "invalid assignment target in assignment")
		
		state.advance()
		state.skipSpace()
		
		lvalue = expr
		span = lvalue.span
		
		rvalue = parseValueExpr(state)
		if rvalue != None:
			span = Span.merge(span, rvalue.span)
		
		expr = Asgn(lvalue, rvalue, infixOp, opTok.span, span)
	
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
	
	return Return(expr, span)

class VarDecl:
	def __init__(self, mut, name, typeRef, expr, span):
		self.mut = mut
		self.name = name
		self.typeRef = typeRef
		self.expr = expr
		self.span = span

def parseVarDecl(state, isConst, isModLevel, isExtern):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	mut = False
	if state.tok.type == TokenType.MUT:
		if isConst:
			logError(state, state.tok.span, '`const` declarations cannot be declared `mut`')
		else:
			mut = True
		state.advance()
		state.skipSpace()
	
	if expectType(state, TokenType.NAME) == False:
		return None
	
	name = Name.fromTok(state.tok)
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
	
	if isModLevel:
		if isExtern and expr != None:
			logError(state, span, 'cannot assign a value in extern declaration')
		elif not isExtern and expr == None:
			logError(state, span, 'non-extern declaration requires a value')
	
	return VarDecl(mut, name, typeRef, expr, span)

def parseLetDecl(state):
	decl = parseVarDecl(state, False, False, False)
	if decl != None:
		decl = LetDecl(decl.name, decl.typeRef, decl.mut, decl.expr, decl.span)
	return decl

def parseStaticDecl(state, doccomment, extern):
	decl = parseVarDecl(state, False, True, extern)
	if decl != None:
		decl = StaticDecl(decl.name, doccomment, extern, 
			decl.mut, decl.typeRef, decl.expr, decl.span)
	return decl

def parseConstDecl(state, doccomment):
	decl = parseVarDecl(state, True, True, False)
	if decl != None:
		decl = ConstDecl(decl.name, doccomment, 
			decl.typeRef, decl.expr, decl.span)
	return decl

# def parseAlias(state, doccomment):
# 	span = state.tok.span
# 	state.advance()
# 	state.skipSpace()
	
# 	name = None
# 	if expectType(state, TokenType.NAME):
# 		name = Name.fromTok(state.tok)
# 		state.advance()
# 		state.skipSpace()
	
# 	typeRef = None
# 	if expectType(state, TokenType.ASGN):
# 		state.advance()
# 		state.skipSpace()
# 		typeRef = parseTypeRef(state)
	
# 	return AliasDecl(name, typeRef, span, doccomment)

def parseTypeDecl(state, doccomment):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	name = None
	if expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		state.advance()
		state.skipSpace()
	
	typeRef = None
	if expectType(state, TokenType.ASGN):
		state.advance()
		state.skipSpace()
		typeRef = parseTypeRef(state)
	
	return TypeDecl(name, typeRef, span, doccomment)

def parseFnDecl(state, doccomment, pub, extern, cconv, traitDecl=False):
	span = state.tok.span
	startLine = state.tok.span.startLine
	onOneLine = True
	
	unsafe = False
	if state.tok.type == TokenType.UNSAFE:
		unsafe = True
		state.advance()
		state.skipSpace()
		if expectType(state, TokenType.FN) == False:
			return None
		
	state.advance()
	state.skipSpace()
	
	name = None
	if expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		span = Span.merge(span, state.tok.span)
		state.advance()
	
	params = []
	cVarArgs = False
	cVarArgsSpan = None
	if expectType(state, TokenType.LPAREN):
		block = parseFnDeclParams(state)
		params = block.list
		span = Span.merge(span, block.span)
		if len(params) > 0 and type(params[-1]) == CVarArgsParam:
			cVarArgsSpan = params.pop().span
			cVarArgs = True
	
	state.skipSpace()
	
	returnType = None
	if state.tok.type == TokenType.ARROW:
		returnType = parseFnDeclReturnType(state)
		if returnType != None:
			span = Span.merge(span, returnType.span)
	
	body = None
	if not (extern or traitDecl):
		blockInfo = parseBlock(state, parseFnBodyExpr)
		body = Block.fromInfo(blockInfo, ScopeType.FN)
		span = Span.merge(span, body.span)
	
	return FnDecl(name, doccomment, pub, extern, cconv, unsafe, 
		params, cVarArgs, returnType, body, span, cVarArgsSpan)

def parseImport(state):
	def parseImportTree(state):
		path = None
		span = state.tok.span
		if state.tok.type == TokenType.NAME:
			path = parsePath(state)
			span = path.span
			path = path.path
		
		imports = None
		rename = None
		if isBlockStart(state):
			imports = parseBlock(state, parseImportTree)
			span = Span.merge(span, imports.span)
			imports = imports.list
		else:
			state.skipSpace()
			if state.tok.type == TokenType.AS:
				state.advance()
				state.skipSpace()
				if expectType(state, TokenType.NAME):
					rename = state.tok
					state.advance()
					state.skipSpace()
		
		if not (path or imports):
			logError(state, state.tok.span, 'expected module path or imports block, found {}'.format(state.tok.type.desc()))
			return None
		
		if path == None:
			path = []
		
		return ImportTree(path, imports, rename, span)
	
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	importTree = parseImportTree(state)
	if importTree == None:
		return None
	
	span = Span.merge(span, importTree.span)
	return Import(importTree, span)

def parseModLevelDecl(state):
	return parseExpr(state, ExprClass.MOD)

def parseLevelImplDecl(state):
	return parseExpr(state, ExprClass.IMPL)

def parseLevelTraitDecl(state):
	return parseExpr(state, ExprClass.TRAIT)

def parseFnBodyExpr(state):
	return parseExpr(state, ExprClass.FN)

class ExprClass(Enum):
	MOD = 'MOD'
	IMPL = 'IMPL'
	TRAIT = 'TRAIT'
	FN = 'FN'
	ATTR = 'ATTR'
	VALUE_EXPR = 'VALUE_EXPR'

MOD_EXPR_TOKS = (
	TokenType.MOD, 
	TokenType.IMPORT, 
	TokenType.FN, 
	TokenType.UNSAFE, 
	TokenType.STATIC, 
	TokenType.STRUCT, 
	TokenType.TUPLE, 
	TokenType.ENUM, 
	TokenType.UNION, 
	TokenType.CONST, 
	# TokenType.ALIAS, 
	TokenType.TYPE, 
	TokenType.IMPL, 
	TokenType.TRAIT, 
	TokenType.PUB, 
	TokenType.ATAT
)

IMPL_EXPR_TOKS = (
	# TokenType.ALIAS, 
	TokenType.FN, 
	TokenType.UNSAFE, 
	TokenType.STATIC, 
	TokenType.STRUCT, 
	TokenType.TUPLE, 
	TokenType.ENUM, 
	TokenType.UNION, 
	TokenType.CONST, 
	TokenType.TYPE,
	TokenType.PUB
)

TRAIT_EXPR_TOKS = (
	TokenType.FN, 
	TokenType.UNSAFE, 
	TokenType.STATIC, 
	TokenType.CONST, 
	TokenType.TYPE
)

VALUE_EXPR_TOKS = (
	TokenType.NEWLINE, 
	TokenType.LBRACE, 
	TokenType.LBRACK, 
	TokenType.LPAREN, 
	TokenType.NAME, 
	TokenType.STRING, 
	TokenType.CHAR, 
	TokenType.INTEGER, 
	TokenType.FLOAT, 
	TokenType.PLUS, 
	TokenType.MINUS, 
	TokenType.AMP, 
	TokenType.AND, 
	TokenType.FALSE, 
	TokenType.TRUE, 
	TokenType.IF, 
	TokenType.VOID, 
	TokenType.UNSAFE, 
	TokenType.STRUCT, 
	TokenType.TUPLE, 
	TokenType.UNION, 
	TokenType.BORROW, 
	TokenType.SIZEOF, 
	TokenType.OFFSETOF
)

FN_EXPR_TOKS = (
	*VALUE_EXPR_TOKS, 
	# TokenType.ALIAS, 
	TokenType.LET, 
	TokenType.LOOP, 
	TokenType.WHILE, 
	# TokenType.FOR, 
	# TokenType.FN, 
	# TokenType.UNSAFE, 
	# TokenType.CONST, 
	# TokenType.STRUCT, 
	# TokenType.TUPLE, 
	# TokenType.ENUM, 
	# TokenType.UNION, 
	TokenType.CONTINUE, 
	TokenType.BREAK, 
	TokenType.RETURN
)

ASGN_OPER_TOKS = (
	TokenType.ASGN, 
	TokenType.TIMESASGN, 
	TokenType.DIVASGN, 
	TokenType.MODULOASGN, 
	TokenType.PLUSASGN, 
	TokenType.MINUSASGN,
	TokenType.ANDASGN, 
	TokenType.ORASGN, 
	TokenType.XORASGN
)

class ModBlockInfo:
	def __init__(self, extern, cconv, pub, attrs):
		self.extern = extern
		self.cconv = cconv
		self.pub = pub
		self.attrs = attrs

def parseExpr(state, exprClass, precedence=0, noSkipSpace=False, allowSimpleFnCall=False, modBlock=None):
	startToken = None
	doccomment = None
	if state.tok.type == TokenType.DOCCOMMENT:
		startToken = doccomment = state.tok
		state.advance()
		permitLineBreak(state)
	
	attrs = []
	if state.tok.type == TokenType.AT:
		if startToken == None:
			startToken = state.tok
		attrs = parseAttrs(state)
		permitLineBreak(state)
	
	pub = False
	if exprClass in (ExprClass.MOD, ExprClass.IMPL) and state.tok.type == TokenType.PUB:
		pub = True
		state.advance()
		state.skipSpace()
		expectType(state, 
			TokenType.UNSAFE, TokenType.FN, TokenType.STRUCT, TokenType.TRAIT, 
			TokenType.UNION, TokenType.EXTERN, TokenType.TUPLE)
	
	extern = False
	cconv = CConv.CAESAR
	if exprClass != ExprClass.FN and state.tok.type == TokenType.EXTERN:
		if startToken == None:
			startToken = state.tok
		extern = True
		state.advance()
		state.skipSpace()
		if state.tok.type == TokenType.STRING:
			if state.tok.content == '"C"':
				cconv = CConv.C
			else:
				logError(state, state.tok.span, '{}: calling convention unsupported'.format(state.tok.content))
			state.advance()
			state.skipSpace()
	
	if (attrs or pub or extern) and isBlockStart(state):
		def parseExprSameType(s):
			return parseExpr(s, exprClass, modBlock=ModBlockInfo(extern, cconv, pub, attrs))
		return parseBlock(state, parseExprSameType)
	
	if modBlock:
		if isIndentDecrease(state):
			return None
		extern = extern or modBlock.extern
		pub = pub or modBlock.pub
		cconv = CConv.C if cconv == CConv.C or modBlock.cconv == CConv.C else CConv.Caesar
		attrs.extend(modBlock.attrs)
	
	if extern and expectType(state, TokenType.FN, TokenType.STATIC, TokenType.UNSAFE) == False:
		return None
	
	if startToken == None: startToken = state.tok
	
	if exprClass == ExprClass.MOD:
		if state.tok.type not in MOD_EXPR_TOKS:
			logError(state, state.tok.span, 'expected declaration, found {}'.format(state.tok.type.desc()))
			return None
	elif exprClass == ExprClass.IMPL:
		if state.tok.type not in IMPL_EXPR_TOKS:
			logError(state, state.tok.span, 'expected declaration, found {}'.format(state.tok.type.desc()))
			return None
	elif exprClass == ExprClass.TRAIT:
		if state.tok.type not in TRAIT_EXPR_TOKS:
			logError(state, state.tok.span, 'expected declaration, found {}'.format(state.tok.type.desc()))
			return None
	elif exprClass == ExprClass.FN:
		if state.tok.type not in FN_EXPR_TOKS:
			logError(state, state.tok.span, 'expected expression, found {}'.format(state.tok.type.desc()))
			return None
	elif exprClass == ExprClass.VALUE_EXPR:
		if state.tok.type not in VALUE_EXPR_TOKS:
			logError(state, state.tok.span, 'expected value expression, found {}'.format(state.tok.type.desc()))
			return None
	elif exprClass == ExprClass.ATTR:
		if state.tok.type not in (*MOD_EXPR_TOKS, *FN_EXPR_TOKS):
			logError(state, state.tok.span, 'expected expression, found {}'.format(state.tok.type.desc()))
			return None
	else:
		assert 0
	
	decl = None
	if state.tok.type == TokenType.ATAT:
		decl = parseAttr(state, True)
	elif state.tok.type == TokenType.MOD:
		decl = parseModule(state, doccomment)
	elif state.tok.type == TokenType.IMPORT:
		decl = parseImport(state)
	elif state.tok.type == TokenType.IMPL:
		decl = parseImpl(state, doccomment)
	elif state.tok.type == TokenType.TRAIT:
		decl = parseTrait(state, doccomment, pub)
	# elif state.tok.type == TokenType.ALIAS:
		# decl = parseAlias(state, doccomment)
	elif state.tok.type == TokenType.TYPE:
		if exprClass == ExprClass.TRAIT:
			decl = parseTraitTypeDecl(state, doccomment)
		else:
			decl = parseTypeDecl(state, doccomment)
	elif state.tok.type == TokenType.FN or \
		state.tok.type == TokenType.UNSAFE and exprClass in (ExprClass.MOD, ExprClass.IMPL, ExprClass.TRAIT):
		decl = parseFnDecl(state, doccomment, pub, extern, cconv, exprClass == ExprClass.TRAIT)
	elif state.tok.type == TokenType.STATIC:
		if exprClass == ExprClass.TRAIT:
			decl = parseTraitStaticDecl(state, doccomment, extern)
		else:
			decl = parseStaticDecl(state, doccomment, extern)
	elif state.tok.type == TokenType.TUPLE:
		decl = parseTupleDecl(state, doccomment, False, pub)
	elif state.tok.type == TokenType.STRUCT:
		decl = parseStructDecl(state, doccomment, False, pub)
	elif state.tok.type == TokenType.UNION:
		decl = parseUnionDecl(state, doccomment, False, pub)
	elif state.tok.type == TokenType.ENUM:
		decl = parseEnumDecl(state, doccomment)
	elif state.tok.type == TokenType.CONST:
		if exprClass == ExprClass.TRAIT:
			decl = parseTraitConstDecl(state, doccomment)
		else:
			decl = parseConstDecl(state, doccomment)
	elif state.tok.type in VALUE_EXPR_TOKS:
		if exprClass == ExprClass.VALUE_EXPR:
			decl = parseValueExprImpl(state, precedence, noSkipSpace, allowSimpleFnCall)
		else:
			decl = parseValueExprOrAsgn(state)
	elif state.tok.type == TokenType.RETURN:
		decl = parseReturn(state)
	elif state.tok.type == TokenType.LET:
		decl = parseLetDecl(state)
	elif state.tok.type == TokenType.LOOP:
		decl = parseLoop(state)
	elif state.tok.type == TokenType.WHILE:
		decl = parseWhile(state)
	# elif state.tok.type == TokenType.FOR:
	# 	decl = parseFor(state)
	elif state.tok.type == TokenType.BREAK or state.tok.type == TokenType.CONTINUE:
		decl = parseLoopCtl(state)
	else:
		assert 0
	
	if decl != None:
		decl.span = Span.merge(startToken.span, decl.span)
		decl.attrs = attrs
	
	return decl

def parseImpl(state, doccomment):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	if expectType(state, TokenType.NAME):
		path = parsePath(state).path
		state.skipSpace()
	
	traitPath = None
	if state.tok.type == TokenType.COLON:
		state.advance()
		state.skipSpace()
		if expectType(state, TokenType.NAME):
			traitPath = parsePath(state).path
			state.skipSpace()
	
	if state.tok.type in (IMPL_EXPR_TOKS):
		decl = parseLevelImplDecl(state)
		decls = [decl]
		span = Span.merge(span, decl.span)
	else:
		block = parseBlock(state, parseLevelImplDecl)
		decls = block.list
		span = Span.merge(span, block.span)
	
	impl = Impl(path, traitPath, doccomment, decls, span)
	return impl

def parseTrait(state, doccomment, pub):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	name = None
	if expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		state.advance()
		state.skipSpace()
	
	if state.tok.type in (TRAIT_EXPR_TOKS):
		decl = parseLevelTraitDecl(state)
		decls = [decl]
		span = Span.merge(span, decl.span)
	else:
		block = parseBlock(state, parseLevelTraitDecl)
		decls = block.list
		span = Span.merge(span, block.span)
	
	trait = TraitDecl(name, doccomment, pub, decls, span)
	return trait

def parseModule(state, doccomment):
	span = state.tok.span
	state.advance()
	state.skipSpace()
	
	name = None
	if expectType(state, TokenType.NAME):
		name = Name.fromTok(state.tok)
		state.advance()
	
	block = parseBlock(state, parseModLevelDecl)
	mod = Mod(name, doccomment, block.list, Span.merge(span, block.span))
	return mod

def parseTopLevelModule(state):
	fileName = path.basename(state.source.fileName)
	name = re.match(r"^([^.]+)?", fileName)[1]
	
	if not re.match(r"^[a-zA-Z_][0-9a-zA-Z_]*$", name):
		logError(state, Span(state.source, 1, 1, 1, 1), 'illegal module name: `{}`'.format(name))
	
	block = parseBlock(state, parseModLevelDecl, topLevelBlock=True)
	mod = Mod(Name(name, block.span.startSpan()), None, block.list, block.span)
	mod.topLevel = True
	return mod

__exit = exit
def exit(_):
	__exit(0)

def parse(source, tokens):
	state = ParserState(source, tokens)
	mod = parseTopLevelModule(state)
	
	if state.failed:
		exit(1)
	
	return mod