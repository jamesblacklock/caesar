from .span import revealSpan, AnsiColor

def logError(state, span, message):
	state.failed = True
	print(revealSpan(span, message))

def logWarning(state, span, message):
	print(revealSpan(span, 'WARNING: ' + message, color=AnsiColor.GRAY))

def logExplain(state, span, message):
	print(revealSpan(span, message, color=AnsiColor.BLUE))
