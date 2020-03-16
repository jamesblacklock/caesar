from enum import Enum

class TokenType(Enum):
	SPACE = 'SPACE'
	COMMENT = 'COMMENT'
	DOCCOMMENT = 'DOCCOMMENT'
	NEWLINE = 'NEWLINE'
	INDENT = 'INDENT'
	AT = 'AT'
	LPAREN = 'LPAREN'
	RPAREN = 'RPAREN'
	LBRACE = 'LBRACE'
	RBRACE = 'RBRACE'
	LBRACK = 'LBRACK'
	RBRACK = 'RBRACK'
	COLON = 'COLON'
	DOT = 'DOT'
	PATH = 'PATH'
	CARET = 'CARET'
	COMMA = 'COMMA'
	ELLIPSIS = 'ELLIPSIS'
	ARROW = 'ARROW'
	LSHIFT = 'LSHIFT'
	RSHIFT = 'RSHIFT'
	TIMES = 'TIMES'
	DIV = 'DIV'
	MODULO = 'MODULO'
	PLUS = 'PLUS'
	MINUS = 'MINUS'
	AMP = 'AMP'
	PIPE = 'PIPE'
	RNGOPEN = 'RNGOPEN'
	EQ = 'EQ'
	NEQ = 'NEQ'
	GREATER = 'GREATER'
	LESS = 'LESS'
	GREATEREQ = 'GREATEREQ'
	LESSEQ = 'LESSEQ'
	AND = 'AND'
	OR = 'OR'
	ASGN = 'ASGN'
	SEMICOLON = 'SEMICOLON'
	INTEGER = 'INTEGER'
	FLOAT = 'FLOAT'
	STRING = 'STRING'
	CHAR = 'CHAR'
	EXTERN = 'EXTERN'
	FN = 'FN'
	MOD = 'MOD'
	STRUCT = 'STRUCT'
	LET = 'LET'
	MUT = 'MUT'
	RETURN = 'RETURN'
	IF = 'IF'
	ELSE = 'ELSE'
	WHILE = 'WHILE'
	FOR = 'FOR'
	CONTINUE = 'CONTINUE'
	BREAK = 'BREAK'
	LOOP = 'LOOP'
	ENUM = 'ENUM'
	PUB = 'PUB'
	UNION = 'UNION'
	CONST = 'CONST'
	STATIC = 'STATIC'
	MATCH = 'MATCH'
	AS = 'AS'
	VOID = 'VOID'
	TRUE = 'TRUE'
	FALSE = 'FALSE'
	NAME = 'NAME',
	UNKNOWN = 'UNKNOWN'
	EOF = 'EOF'
	
	def desc(self):
		if self == TokenType.SPACE:
			return 'space'
		elif self == TokenType.COMMENT:
			return 'comment'
		elif self == TokenType.DOCCOMMENT:
			return 'doc comment'
		elif self == TokenType.NEWLINE:
			return 'line break'
		elif self == TokenType.INDENT:
			return 'indentation'
		elif self == TokenType.AT:
			return '`@`'
		elif self == TokenType.LPAREN:
			return '`(`'
		elif self == TokenType.RPAREN:
			return '`)`'
		elif self == TokenType.LBRACE:
			return '`{`'
		elif self == TokenType.RBRACE:
			return '`}`'
		elif self == TokenType.LBRACK:
			return '`[`'
		elif self == TokenType.RBRACK:
			return '`]`'
		elif self == TokenType.COLON:
			return '`:`'
		elif self == TokenType.DOT:
			return '`.`'
		elif self == TokenType.PATH:
			return '`::`'
		elif self == TokenType.CARET:
			return '`^`'
		elif self == TokenType.COMMA:
			return '`,`'
		elif self == TokenType.ELLIPSIS:
			return '`...`'
		elif self == TokenType.ARROW:
			return '`->`'
		elif self == TokenType.LSHIFT:
			return '`<<`'
		elif self == TokenType.RSHIFT:
			return '`>>`'
		elif self == TokenType.TIMES:
			return '`*`'
		elif self == TokenType.DIV:
			return '`/`'
		elif self == TokenType.MODULO:
			return '`%`'
		elif self == TokenType.PLUS:
			return '`+`'
		elif self == TokenType.MINUS:
			return '`-`'
		elif self == TokenType.AMP:
			return '`&`'
		elif self == TokenType.PIPE:
			return '`|`'
		elif self == TokenType.RNGOPEN:
			return '`..<`'
		elif self == TokenType.EQ:
			return '`==`'
		elif self == TokenType.NEQ:
			return '`!=`'
		elif self == TokenType.GREATER:
			return '`>`'
		elif self == TokenType.LESS:
			return '`<`'
		elif self == TokenType.GREATEREQ:
			return '`>=`'
		elif self == TokenType.LESSEQ:
			return '`<=`'
		elif self == TokenType.AND:
			return '`&&`'
		elif self == TokenType.OR:
			return '`||`'
		elif self == TokenType.ASGN:
			return '`=`'
		elif self == TokenType.SEMICOLON:
			return '`;`'
		elif self == TokenType.FLOAT:
			return 'float literal'
		elif self == TokenType.INTEGER:
			return 'integer literal'
		elif self == TokenType.STRING:
			return 'string literal'
		elif self == TokenType.CHAR:
			return 'character literal'
		elif self == TokenType.EXTERN:
			return '`extern`'
		elif self == TokenType.FN:
			return '`fn`'
		elif self == TokenType.MOD:
			return '`mod`'
		elif self == TokenType.STRUCT:
			return '`struct`'
		elif self == TokenType.RETURN:
			return '`return`'
		elif self == TokenType.LET:
			return '`let`'
		elif self == TokenType.MUT:
			return '`mut`'
		elif self == TokenType.IF:
			return '`if`'
		elif self == TokenType.ELSE:
			return '`else`'
		elif self == TokenType.WHILE:
			return '`while`'
		elif self == TokenType.FOR:
			return '`for`'
		elif self == TokenType.BREAK:
			return '`break`'
		elif self == TokenType.CONTINUE:
			return '`continue`'
		elif self == TokenType.LOOP:
			return '`loop`'
		elif self == TokenType.ENUM:
			return '`enum`'
		elif self == TokenType.PUB:
			return '`pub`'
		elif self == TokenType.UNION:
			return '`union`'
		elif self == TokenType.CONST:
			return '`const`'
		elif self == TokenType.STATIC:
			return '`static`'
		elif self == TokenType.MATCH:
			return '`match`'
		elif self == TokenType.AS:
			return '`as`'
		elif self == TokenType.VOID:
			return '`void`'
		elif self == TokenType.TRUE:
			return '`true`'
		elif self == TokenType.FALSE:
			return '`false`'
		elif self == TokenType.NAME:
			return 'name'
		elif self == TokenType.UNKNOWN:
			return '<unknown>'
		elif self == TokenType.EOF:
			return '<end-of-file>'
		else:
			assert False

class Token:
	def __init__(self, span, type, content, offset=None):
		self.span = span
		self.type = type
		self.content = content
		self.error = None
		self.offset = offset
	
	def __str__(self):
		return '{} ({})'.format(self.type, self.content[:10])
	
	def __repr__(self):
		return self.__str__()
