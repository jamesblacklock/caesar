class CsrError(Exception):
    pass

class CsrSyntaxError(CsrError):
	def __init__(self, message):
		super().__init__('\033[31m\033[1merror\033[0m\033[1m: syntax error\033[0m\n{}'.format(message))

class CsrCompileError(CsrError):
	def __init__(self, message):
		super().__init__('\033[31m\033[1merror\033[0m\033[1m: compile error\033[0m\n{}'.format(message))
