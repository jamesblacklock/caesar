#! /usr/local/bin/python3

from caesar.exception                    import CsrError
from caesar.sourcefile                   import SourceFile
from caesar.span                         import revealSpan, Span
from caesar.token                        import TokenType, revealToken
from caesar.tokenizer                    import tokenize
from caesar.parser                       import parse
from caesar.analyzer                     import analyze
from caesar.generator                    import generateIR, generateAsm

def main():
	source = SourceFile('test.csr')
	
	try:
		tokens = tokenize(source)
		module = parse(source, tokens)
		# analyze(module)
		# generateIR(module)
		# asm = generateAsm(module)
	except CsrError as e:
		print(e)
		return
	
	# for token in tokens:
	# 	if token.type == TokenType.NEWLINE or token.type == TokenType.SPACE:
	# 		continue
	# 	print('\033[2J\033[H', revealToken(source, token), sep='')
	# 	input()

	# for decl in ast:
	# 	print('\033[2J\033[H', decl, sep='')
	# 	input()
	
	# for decl in module.symbolTable.values():
	# 	print(str(decl.ir).replace('\t', '    ')) 
	
	# print(asm.replace('\t', '    '))

if __name__ == '__main__':
	main()
