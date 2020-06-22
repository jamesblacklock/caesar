import re
from enum   import Enum
from .span  import Span
from .token import *
from .log   import logError

class IndentMode(Enum):
	SPACE = 'SPACE'
	TAB = 'TAB'

class TestState:
	def __init__(self):
		self.done = False

class LexerState:
	def __init__(self, source):
		self.source = source
		self.content = source.content
		self.offset = 0
		self.line = 1
		self.column = 1
		self.error = None
		self.errorToken = None
		self.indentMode = None
		self.failed = False
	
	@property
	def char(self):
		return self.content[self.offset] if self.offset < len(self.content) else ''
	
	@property
	def nextChar(self):
		return self.content[self.offset+1] if self.offset+1 < len(self.content) else ''
	
	def advance(self, count=1):
		for i in range(0, count):
			if self.offset >= len(self.content):
				return
			
			if self.content[self.offset] == '\n':
				self.line += 1
				self.column = 1
			else:
				self.column += 1
			
			self.offset += 1

def singleCharTest(state, testState):
	if testState.data:
		testState.done = True
	testState.data = True

def lexToken(state, type, test=singleCharTest, testState=None, advance=0):
	if testState == None:
		testState = TestState()
		testState.data = None
	
	contentStart = state.offset
	startLine = state.line
	startColumn = state.column
	endLine = startLine
	endColumn = startColumn
	
	state.advance(advance)
	
	while True:
		test(state, testState)
		if testState.done == True or state.char == '':
			break
		endLine = state.line
		endColumn = state.column
		state.advance()
	
	tok = Token(Span(state.source, startLine, startColumn, endLine, endColumn), 
		type, state.content[contentStart:state.offset])
	
	if not testState.done:
		state.error = 'found unexpected <end-of-file> while parsing'
		state.errorToken = tok
	
	return tok

def lexSpace(state):
	def testSpace(state, testState):
		if state.char != ' ' and state.char != '\t':
			testState.done = True
	
	def testSpaceIndent(state, testState):
		if state.char != ' ':
			testState.done = True
	
	def testTabIndent(state, testState):
		if state.char != '\t':
			testState.done = True
	
	isIndent = state.column == 1
	if isIndent:
		if state.indentMode == None:
			state.indentMode = IndentMode.SPACE if state.char == ' ' else IndentMode.TAB
		if state.indentMode == IndentMode.SPACE:
			regex = r"^ +$"
			expected = 'spaces'
			found = 'tabs'
		else:
			regex = r"^\t+$"
			expected = 'tabs'
			found = 'spaces'
		test = testSpaceIndent if state.char == ' ' else testTabIndent
	else:
		test = testSpace
	
	tok = lexToken(state, TokenType.INDENT if isIndent else TokenType.SPACE, test)
	
	if isIndent and not re.match(regex, tok.content):
		state.error = 'inconsistent indentation; expected {}, found {}'.format(expected, found)
		state.errorToken = tok
	
	return tok

def lexComment(state):
	def singleLineTest(state, testState):
		if state.char == '\n' or state.char == '':
			testState.done = True
	
	def multiLineTest(state, testState):
		if testState.nest == 0:
			testState.done = True
		
		if testState.lastChar == '#' and state.char == '=':
			testState.nest += 1
			testState.lastChar = ''
		elif testState.lastChar == '=' and state.char == '#':
			testState.nest -= 1
			testState.lastChar = ''
		else:
			testState.lastChar = state.char
	
	if state.offset+1 < len(state.content) and \
	state.char == '#' and \
	state.content[state.offset+1] == '=':
		testState = TestState()
		testState.nest = 1
		testState.lastChar = ''
		doc = state.offset+2 < len(state.content) and state.content[state.offset+2] == '='
		return lexToken(state, TokenType.DOCCOMMENT if doc else TokenType.COMMENT, multiLineTest, testState, advance=2)
	else:
		doc = state.offset+1 < len(state.content) and state.content[state.offset+1] == '#'
		return lexToken(state, TokenType.DOCCOMMENT if doc else TokenType.COMMENT, singleLineTest)

def lexNewline(state):
	def test(state, testState):
		if state.char != '\n':
			testState.done = True
	
	return lexToken(state, TokenType.NEWLINE, test)

def lexOperator(state):
	def test(state, testState):
		testState.lengthSoFar += 1
		if testState.lengthSoFar > testState.length:
			testState.done = True
	
	operators = \
	[
		('...', TokenType.ELLIPSIS),
		('..<', TokenType.RNGOPEN),
		('*=', TokenType.TIMESASGN),
		('/=', TokenType.DIVASGN),
		('%=', TokenType.MODULOASGN),
		('+=', TokenType.PLUSASGN),
		('-=', TokenType.MINUSASGN),
		('>=', TokenType.GREATEREQ),
		('<=', TokenType.LESSEQ),
		('&&', TokenType.AND),
		('||', TokenType.OR),
		('==', TokenType.EQ),
		('!=', TokenType.NEQ),
		('->', TokenType.ARROW),
		('<<', TokenType.LSHIFT),
		('>>', TokenType.RSHIFT),
		('::', TokenType.PATH),
		('@@', TokenType.ATAT),
		('^.', TokenType.DEREFDOT),
		('@', TokenType.AT),
		('.', TokenType.DOT),
		('(', TokenType.LPAREN),
		(')', TokenType.RPAREN),
		('{', TokenType.LBRACE),
		('}', TokenType.RBRACE),
		('[', TokenType.LBRACK),
		(']', TokenType.RBRACK),
		(':', TokenType.COLON),
		('^', TokenType.CARET),
		(',', TokenType.COMMA),
		('*', TokenType.TIMES),
		('/', TokenType.DIV),
		('%', TokenType.MODULO),
		('+', TokenType.PLUS),
		('-', TokenType.MINUS),
		('&', TokenType.AMP),
		('|', TokenType.PIPE),
		('>', TokenType.GREATER),
		('<', TokenType.LESS),
		('=', TokenType.ASGN),
		(';', TokenType.SEMICOLON)
	]
	
	for (op, type) in operators:
		if state.content[state.offset:state.offset+len(op)] == op:
			testState = TestState()
			testState.lengthSoFar = 0
			testState.length = len(op)
			return lexToken(state, type, test, testState)
	
	tok = lexToken(state, TokenType.UNKNOWN)
	state.error = 'invalid operator'
	state.errorToken = tok
	return tok

def lexNumber(state):
	def test(state, testState):
		if state.char == '.':
			if state.nextChar == '.':
				testState.done = True
				return
			else:
				testState.isFloat = True
		
		if testState.isFloat and (state.char == 'e' or state.char == 'E' or \
			state.char == 'p' or state.char == 'P'):
			testState.expectExponent = True
		elif not re.match(r"[\d_a-zA-Z.]", state.char):
			if testState.expectExponent and (state.char == '+' or state.char == '-'):
				pass
			else:
				testState.done = True
	
	testState = TestState()
	testState.isFloat = False
	testState.expectExponent = False
	tok = lexToken(state, TokenType.INTEGER, test, testState)
	
	if re.match(r".*\.", tok.content):
		decFloat = r"([\d_]*\.[\d_]*)([eE][\-+]?[\d_]+)?"
		hexFloat = r"0x([\da-fA-F_]*\.[\da-fA-F_]*)([pP][\-+]?[\d_]+)?"
		suffix   = r"(f32|f64)?"
		regex    = r"^({}|{}){}$".format(decFloat, hexFloat, suffix)
		
		if not re.match(regex, tok.content):
			state.error = 'invalid floating point literal'
			state.errorToken = tok
		
		tok.type = TokenType.FLOAT
	else:
		decInt = r"([\d_]+)"
		binInt = r"(0b[01_]+)"
		hexInt = r"(0x[\da-fA-F_]+)"
		suffix = r"(i8|u8|i16|u16|i32|u32|i64|u64|sz|usz)?"
		regex  = r"^({}|{}|{}){}$".format(decInt, binInt, hexInt, suffix)
		if not re.match(regex, tok.content):
			state.error = 'invalid integer literal'
			state.errorToken = tok
	
	return tok

def lexString(state):
	def test(state, testState):
		if testState.foundClosingQuote:
			testState.done = True
		if state.char == '\\':
			testState.esc = True
		else:
			if state.char == '"' and not testState.esc:
				testState.foundClosingQuote = True
			testState.esc = False
	
	testState = TestState()
	testState.foundClosingQuote = False
	testState.esc = False
	return lexToken(state, TokenType.STRING, test, testState, advance=1)

def lexChar(state):
	def test(state, testState):
		if testState.foundClosingQuote:
			testState.done = True
		if state.char == '\\':
			testState.esc = True
		else:
			if state.char == '\'' and not testState.esc:
				testState.foundClosingQuote = True
			testState.esc = False
	
	testState = TestState()
	testState.foundClosingQuote = False
	testState.esc = False
	return lexToken(state, TokenType.CHAR, test, testState, advance=1)

def lexNameOrKeyword(state):
	def keywordTest(state, testState):
		testState.lengthSoFar += 1
		if testState.lengthSoFar > testState.length:
			testState.done = True
	
	def nameTest(state, testState):
		if not re.match(r"[a-zA-Z0-9_]", state.char):
			testState.done = True
	
	keywords = \
	[
		('extern', TokenType.EXTERN),
		('fn', TokenType.FN),
		('mod', TokenType.MOD),
		('struct', TokenType.STRUCT),
		('let', TokenType.LET),
		('mut', TokenType.MUT),
		('return', TokenType.RETURN),
		('if', TokenType.IF),
		('else', TokenType.ELSE),
		('while', TokenType.WHILE),
		('for', TokenType.FOR),
		('break', TokenType.BREAK),
		('continue', TokenType.CONTINUE),
		('loop', TokenType.LOOP),
		('enum', TokenType.ENUM),
		('pub', TokenType.PUB),
		('union', TokenType.UNION),
		('const', TokenType.CONST),
		('static', TokenType.STATIC),
		('match', TokenType.MATCH),
		('import', TokenType.IMPORT),
		('impl', TokenType.IMPL),
		('trait', TokenType.TRAIT),
		('unsafe', TokenType.UNSAFE),
		('as', TokenType.AS),
		('is', TokenType.IS),
		('alias', TokenType.ALIAS),
		('type', TokenType.TYPE),
		('owned', TokenType.OWNED),
		('borrow', TokenType.BORROW),
		('sizeof', TokenType.SIZEOF),
		('offsetof', TokenType.OFFSETOF),
		('void', TokenType.VOID),
		('true', TokenType.TRUE),
		('false', TokenType.FALSE)
	]
	
	for (kw, type) in keywords:
		slice = state.content[state.offset:min(state.offset+len(kw), len(state.content))]
		nextChar = state.content[state.offset+len(kw)] if state.offset+len(kw) < len(state.content) else ''
		if slice == kw and not re.match(r"[a-zA-Z0-9_]", nextChar):
			testState = TestState()
			testState.lengthSoFar = 0
			testState.length = len(kw)
			return lexToken(state, type, keywordTest, testState)
	
	return lexToken(state, TokenType.NAME, nameTest)

def lexUnknownToken(state):
	tok = lexToken(state, TokenType.UNKNOWN)
	state.error = 'encountered unexpected character'
	state.errorToken = tok
	return tok

def tokenize(source):
	state = LexerState(source)
	tokens = []
	tok = None
	while state.char != '':
		if state.char == ' ' or state.char == '\t':
			tok = lexSpace(state)
		elif state.char == '#':
			tok = lexComment(state)
		elif state.char == '\n':
			tok = lexNewline(state)
		elif state.char == '.' and tok and tok.type == TokenType.NAME:
			tok = lexOperator(state)
		elif re.match(r"\d", state.char) or state.char == '.' and re.match(r"\d", state.nextChar):
			tok = lexNumber(state)
		elif re.match(r"[\[\]!@\(\):;^&,.\-%+=*/<>|{}]", state.char):
			tok = lexOperator(state)
		elif state.char == '"':
			tok = lexString(state)
		elif state.char == '\'':
			tok = lexChar(state)
		elif re.match(r"[a-zA-Z_]", state.char):
			tok = lexNameOrKeyword(state)
		else:
			tok = lexUnknownToken(state)
		
		if state.error:
			logError(state, state.errorToken.span, state.error)
			state.error = None
		
		tok.offset = len(tokens)
		tokens.append(tok)
	
	if state.failed:
		exit(1)
	
	tokens.append(Token(Span(source, state.line, state.column, state.line, state.column), 
		TokenType.EOF, '', len(tokens)))
	
	return tokens
