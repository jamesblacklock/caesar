from re import findall

class Span:
	def __init__(self, source, startLine, startColumn, endLine, endColumn, zeroWidth=False):
		self.source = source
		self.startLine = startLine
		self.startColumn = startColumn
		self.endLine = endLine
		self.endColumn = endColumn
		self.zeroWidth = zeroWidth
	
	@staticmethod
	def merge(span1, span2):
		if span1 == None: return span2
		if span2 == None: return span1
		zeroWidth = \
			span1.startLine == span2.startLine and span1.startColumn == span2.startColumn and \
			span1.endLine == span2.endLine     and span1.endColumn == span2.endColumn and \
			span1.zeroWidth and span2.zeroWidth
		return Span(span1.source, span1.startLine, span1.startColumn, span2.endLine, span2.endColumn, zeroWidth)
	
	def clone(self):
		return Span(self.source, self.startLine, self.startColumn, self.endLine, self.endColumn, self.zeroWidth)
	
	def startSpan(self):
		other = self.clone()
		other.endLine = other.startLine
		other.endColumn = other.startColumn
		return other
	
	def endSpan(self):
		other = self.clone()
		other.zeroWidth = True
		other.startLine = other.endLine
		other.startColumn = other.endColumn
		return other
	
	# def advance(self):
	# 	self.endColumn += 1
	# 	line = self.source.lines[self.startLine]
	# 	if len(line) < self.endColumn and len(self.source.lines) >= self.endLine+1:
	# 		self.endLine += 1
	# 		self.endColumn = 1
		
	# 	self.startLine = self.endLine
	# 	self.startColumn = self.endColumn
	# 	return self
	
	def reveal(self):
		print(revealSpan(self))

class AnsiColor:
	BLACK = '30'
	RED = '31'
	GREEN = '32'
	YELLOW = '33'
	BLUE = '34'
	MAGENTA = '35'
	CYAN = '36'
	WHITE = '37'
	GRAY = '0;1;2'

def revealSpan(span, message='', leadingLines=2, followingLines=2, indicatorChar='^', color=AnsiColor.RED):
	source = span.source
	gutterPadding = ' ' if span.startLine == span.endLine else '   '
	
	# print file info
	output = ['\033[34;1m   --> \033[0m{}:{}:{}\n'.format(source.fileName, span.startLine, span.startColumn)]
	
	# print leading lines
	line = max(1, span.startLine-leadingLines)
	while line < span.startLine:
		output.append('\033[34;1m{:>4}|{}\033[0m'.format(line, gutterPadding)) # gutter
		output.append(source.lines[line-1]) # content
		output.append('\n')
		line += 1
	
	# print lines containing span
	output.append('\033[34;1m{:>4}|{}\033[0m'.format(line, gutterPadding)) # gutter
	output.append(source.lines[line-1][:span.startColumn-1]) # leading content
	
	if not span.zeroWidth:
		output.append('\033[{};1m'.format(color)) # color
	
	# span content
	if span.startLine == span.endLine:
		output.append(source.lines[line-1][span.startColumn-1:span.endColumn])
	else:
		output.append(source.lines[line-1][span.startColumn-1:]) # first line content
		output.append('\033[0m\n')
		tabCount = len(findall('\t', source.lines[span.startLine-1][:span.startColumn-1]))
		output.append('\033[34;1m    |  \033[{}m_'.format(color))
		output.append('_' * (span.startColumn - tabCount + 4*tabCount - 1))
		output.append('{}\033[0m\n'.format(indicatorChar))
		line += 1
		while line < span.endLine:
			output.append('\033[34;1m{:>4}|\033[{};1m | '.format(line, color)) # gutter
			output.append(source.lines[line-1]) # content
			output.append('\033[0m\n')
			line += 1
		output.append('\033[34;1m{:>4}|\033[{};1m | '.format(line, color)) # gutter
		output.append(source.lines[span.endLine-1][:span.endColumn]) # last line content
	
	output.append('\033[0m') # revert color
	output.append(source.lines[line-1][span.endColumn:]) # following content
	output.append('\n')
	line += 1
	
	if span.startLine == span.endLine:
		tabCount = len(findall('\t', source.lines[span.startLine-1][:span.startColumn-1]))
		output.append('\033[34;1m    |{}\033[{}m'.format(gutterPadding, color))
		output.append(' ' * (span.startColumn - tabCount + 4*tabCount - 1))
		tabCount = len(findall('\t', source.lines[span.startLine-1][span.startColumn-1:span.endColumn]))
		output.append(indicatorChar * max(1, span.endColumn-span.startColumn - tabCount + 4*tabCount + 1))
		output.append(' ' + message)
		output.append('\033[0m\n')
	else:
		tabCount = len(findall('\t', source.lines[span.endLine-1][:span.endColumn]))
		output.append('\033[34;1m    |\033[{}m |_'.format(color))
		output.append('_' * (span.endColumn - tabCount + 4*tabCount - 1))
		output.append('{} {}'.format(indicatorChar, message))
		output.append('\033[0m\n')
	
	endLine = min(len(source.lines), span.endLine+followingLines)
	while line <= endLine:
		output.append('\033[34;1m{:>4}|{}\033[0m'.format(line, gutterPadding))
		output.append(source.lines[line-1])
		output.append('\n')
		line += 1
	
	return ''.join(output).replace('\t', '    ')
