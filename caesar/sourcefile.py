class SourceFile:
	def __init__(self, fileName):
		file = open(fileName, encoding='utf-8')
		self.fileName = fileName
		self.content =  file.read()
		self.lines = self.content.split('\n')
		file.close()
