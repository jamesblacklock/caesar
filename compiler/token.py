from enum import Enum
from .infixops import InfixOps

class TokenType(Enum):
	SPACE      = 'space'
	COMMENT    = 'comment'
	DOCCOMMENT = 'doc comment'
	NEWLINE    = 'line break'
	INDENT     = 'indentation'
	AT         = '`@`'
	ATAT       = '`@@`'
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
	DEREFDOT   = '`^.`'
	TIMES      = '`*`'
	DIV        = '`/`'
	MODULO     = '`%`'
	PLUS       = '`+`'
	MINUS      = '`-`'
	AMP        = '`&`'
	PIPE       = '`|`'
	TIMESASGN  = '`*=`'
	DIVASGN    = '`/=`'
	MODULOASGN = '`%=`'
	PLUSASGN   = '`+=`'
	MINUSASGN  = '`-=`'
	ANDASGN    = '`&=`'
	ORASGN     = '`|=`'
	XORASGN    = '`^=`'
	RNGOPEN    = '`..<`'
	EQ         = '`==`'
	NEQ        = '`!=`'
	GREATER    = '`>`'
	LESS       = '`<`'
	GREATEREQ  = '`>=`'
	LESSEQ     = '`<=`'
	ASGN       = '`=`'
	SEMICOLON  = '`;`'
	FLOAT      = 'float literal'
	INTEGER    = 'integer literal'
	STRING     = 'string literal'
	CHAR       = 'character literal'
	AND        = '`and`'
	OR         = '`or`'
	EXTERN     = '`extern`'
	FN         = '`fn`'
	MOD        = '`mod`'
	STRUCT     = '`struct`'
	TUPLE      = '`tuple`'
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
	OWNED      = '`owned`'
	BORROW     = '`borrow`'
	AS         = '`as`'
	IS         = '`is`'
	OFFSETOF   = '`offsetof`'
	SIZEOF     = '`sizeof`'
	VOID       = '`void`'
	TRUE       = '`true`'
	FALSE      = '`false`'
	ALIAS      = '`alias`'
	TYPE       = '`type`'
	NAME       = 'name'
	UNKNOWN    = '<unknown>'
	EOF        = '<end-of-file>'
	
	def desc(self):
		return self.value

class Token:
	def __init__(self, span, type, content, offset=None):
		self.span = span
		self.type = type
		self.content = content
		self.offset = offset
	
	def __str__(self):
		return '{} ({})'.format(self.type, self.content[:10])
	
	def __repr__(self):
		return self.__str__()
	
	def toInfixOp(self):
		if self.type == TokenType.ARROW:
			return InfixOps.ARROW
		elif self.type == TokenType.LSHIFT:
			return InfixOps.LSHIFT
		elif self.type == TokenType.RSHIFT:
			return InfixOps.RSHIFT
		elif self.type == TokenType.TIMES:
			return InfixOps.TIMES
		elif self.type == TokenType.DIV:
			return InfixOps.DIV
		elif self.type == TokenType.MODULO:
			return InfixOps.MODULO
		elif self.type == TokenType.PLUS:
			return InfixOps.PLUS
		elif self.type == TokenType.MINUS:
			return InfixOps.MINUS
		elif self.type == TokenType.AMP:
			return InfixOps.BITAND
		elif self.type == TokenType.PIPE:
			return InfixOps.BITOR
		elif self.type == TokenType.CARET:
			return InfixOps.BITXOR
		elif self.type == TokenType.ELLIPSIS:
			return InfixOps.RNGCLOSED
		elif self.type == TokenType.RNGOPEN:
			return InfixOps.RNGOPEN
		elif self.type == TokenType.EQ:
			return InfixOps.EQ
		elif self.type == TokenType.NEQ:
			return InfixOps.NEQ
		elif self.type == TokenType.GREATER:
			return InfixOps.GREATER
		elif self.type == TokenType.LESS:
			return InfixOps.LESS
		elif self.type == TokenType.GREATEREQ:
			return InfixOps.GREATEREQ
		elif self.type == TokenType.LESSEQ:
			return InfixOps.LESSEQ
		elif self.type == TokenType.AND:
			return InfixOps.AND
		elif self.type == TokenType.OR:
			return InfixOps.OR
		else:
			return None
