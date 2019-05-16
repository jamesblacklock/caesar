import re
from enum import Enum
from .token import *
from .err import logError

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
	
	@property
	def char(self):
		return self.content[self.offset] if self.offset < len(self.content) else ''
	
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
	
	tok = Token(Span(state.source, startLine, startColumn, endLine, endColumn), type, state.content[contentStart:state.offset])
	
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
		if state.char == '\n':
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
		('..<', TokenType.RNGCLOSED),
		('..=', TokenType.RNGOPEN),
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
		('@', TokenType.AT),
		('.', TokenType.DOT),
		('(', TokenType.LPAREN),
		(')', TokenType.RPAREN),
		('{', TokenType.LBRACE),
		('}', TokenType.RBRACE),
		(':', TokenType.COLON),
		('^', TokenType.CARET),
		(',', TokenType.COMMA),
		('*', TokenType.TIMES),
		('/', TokenType.DIV),
		('%', TokenType.MODULO),
		('+', TokenType.PLUS),
		('-', TokenType.MINUS),
		('&', TokenType.BITAND),
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
		if not re.match(r"[\d_a-zA-Z]", state.char):
			testState.done = True
	
	tok = lexToken(state, TokenType.INTEGER, test)
	if not re.match(r"^[\d_]+(:?i8|u8|i16|u16|i32|u32|i64|u64|sz|usz)?$", tok.content):
		state.error = 'invalid suffix for integer literal'
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
		('as', TokenType.AS),
		('true', TokenType.TRUE),
		('false', TokenType.FALSE)
	]
	
	for (kw, type) in keywords:
		slice = state.content[state.offset:min(state.offset+len(kw), len(state.content)-1)]
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
	while state.char != '':
		tok = None
		if state.char == ' ' or state.char == '\t':
			tok = lexSpace(state)
		elif state.char == '#':
			tok = lexComment(state)
		elif state.char == '\n':
			tok = lexNewline(state)
		elif re.match(r"[!@():^,.\-+=*/<>;{}]", state.char):
			tok = lexOperator(state)
		elif re.match(r"\d", state.char):
			tok = lexNumber(state)
		elif state.char == '"':
			tok = lexString(state)
		elif re.match(r"[a-zA-Z_]", state.char):
			tok = lexNameOrKeyword(state)
		else:
			tok = lexUnknownToken(state)
		
		if state.error:
			logError(state, state.errorToken.span, state.error)
			exit(1)
		
		tok.offset = len(tokens)
		tokens.append(tok)
	
	tokens.append(Token(Span(source, state.line, state.column, state.line, state.column), 
		TokenType.EOF, '', len(tokens)))
	
	return tokens
