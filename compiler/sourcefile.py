import zlib

class SourceFile:
	def __init__(self, fileName):
		file = open(fileName, encoding='utf-8')
		self.fileName = fileName
		self.content =  file.read()
		self.checksum = zlib.crc32(self.content.encode('utf-8'))
		self.lines = self.content.split('\n')
		self.whitespaceAware = True
		file.close()
