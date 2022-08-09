from .span import revealSpan, AnsiColor

def logExplain(state, span, message):
	print(revealSpan(span, message, color=AnsiColor.BLUE))

def logWarning(state, span, message):
	print(revealSpan(span, 'warning: ' + message, color=AnsiColor.YELLOW))

def logError(state, span, message):
	state.failed = True
	print(revealSpan(span, message))
