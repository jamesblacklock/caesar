import re

class ParseRulePart:
	def __init__(self, oneOrMore, zeroOrMore, optional, ignore):
		 self.oneOrMore = oneOrMore
		 self.zeroOrMore = zeroOrMore
		 self.optional = optional
		 self.ignore = ignore
	
	def __repr__(self):
		return str(self)

class T(ParseRulePart):
	def __init__(self, arg, **kwargs):
		super().__init__(self, **kwargs)
		self.arg = arg
	
	def __str__(self):
		return '({})'.format(self.arg)

class P(ParseRulePart):
	def __init__(self, arg, **kwargs):
		super().__init__(self, **kwargs)
		self.arg = arg
	
	def __str__(self):
		return '${}'.format(self.arg)
	
class O(ParseRulePart):
	def __init__(self, *args, **kwargs):
		super().__init__(self, **kwargs)
		self.args = args
	
	def __str__(self):
		return '{}'.format('|'.join([str(arg) for arg in self.args]))

class S(ParseRulePart):
	def __init__(self, *args, **kwargs):
		super().__init__(self, **kwargs)
		self.args = args
	
	def __str__(self):
		return '{}'.format(','.join([str(arg) for arg in self.args]))
	

def generateParser(rules):
	for rule in rules:
		print(rule)
		

generateParser({
	'space': O(T('SPACE'),T('COMMENT')),
	'term[space]: .NEWLINE|.SEMICOLON|.EOF',
	'attr: .AT,.NAME,(space*,.LPAREN,(space*,.STRING,space*,.COMMA)*,(space*,.STRING,.COMMA?)?)|?(.NEWLINE|.SPACE|.COMMENT)']
})