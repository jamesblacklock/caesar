from enum import Enum

class TokenType(Enum):
	SPACE      = 'space'
	COMMENT    = 'comment'
	DOCCOMMENT = 'doc comment'
	NEWLINE    = 'line break'
	INDENT     = 'indentation'
	AT         = '`@`'
	LPAREN     = '`(`'
	RPAREN     = '`)`'
	LBRACE     = '`{`'
	RBRACE     = '`}`'
	LBRACK     = '`[`'
	RBRACK     = '`]`'
	COLON      = '`:`'
	DOT        = '`.`'
	PATH       = '`::`'
	CARET      = '`^`'
	COMMA      = '`,`'
	ELLIPSIS   = '`...`'
	ARROW      = '`->`'
	LSHIFT     = '`<<`'
	RSHIFT     = '`>>`'
	TIMES      = '`*`'
	DIV        = '`/`'
	MODULO     = '`%`'
	PLUS       = '`+`'
	MINUS      = '`-`'
	AMP        = '`&`'
	PIPE       = '`|`'
	RNGOPEN    = '`..<`'
	EQ         = '`==`'
	NEQ        = '`!=`'
	GREATER    = '`>`'
	LESS       = '`<`'
	GREATEREQ  = '`>=`'
	LESSEQ     = '`<=`'
	AND        = '`&&`'
	OR         = '`||`'
	ASGN       = '`=`'
	SEMICOLON  = '`;`'
	FLOAT      = 'float literal'
	INTEGER    = 'integer literal'
	STRING     = 'string literal'
	CHAR       = 'character literal'
	EXTERN     = '`extern`'
	FN         = '`fn`'
	MOD        = '`mod`'
	STRUCT     = '`struct`'
	RETURN     = '`return`'
	LET        = '`let`'
	MUT        = '`mut`'
	IF         = '`if`'
	ELSE       = '`else`'
	WHILE      = '`while`'
	FOR        = '`for`'
	BREAK      = '`break`'
	CONTINUE   = '`continue`'
	LOOP       = '`loop`'
	ENUM       = '`enum`'
	PUB        = '`pub`'
	UNION      = '`union`'
	CONST      = '`const`'
	STATIC     = '`static`'
	MATCH      = '`match`'
	IMPORT     = '`import`'
	IMPL       = '`impl`'
	TRAIT      = '`trait`'
	UNSAFE     = '`unsafe`'
	AS         = '`as`'
	OFFSETOF   = '`offsetof`'
	SIZEOF     = '`sizeof`'
	VOID       = '`void`'
	TRUE       = '`true`'
	FALSE      = '`false`'
	NAME       = 'name'
	UNKNOWN    = '<unknown>'
	EOF        = '<end-of-file>'
	
	def desc(self):
		return self.name

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
