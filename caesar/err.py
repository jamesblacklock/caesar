from .span import revealSpan

def logError(state, span, message):
	state.failed = True
	print(revealSpan(span, message))