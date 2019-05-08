#! /usr/local/bin/python3

import sys

from caesar.exception                    import CsrError
from caesar.sourcefile                   import SourceFile
from caesar.span                         import revealSpan, Span
from caesar.token                        import TokenType, revealToken
from caesar.tokenizer                    import tokenize
from caesar.parser                       import parse
from caesar.analyzer                     import analyze
from caesar.generator                    import generateIR, generateAsm

def main(args):
	if len(args) < 2 or len(args) > 3:
		print('Usage: caesar <input filename> [<output filename>]')
		exit(1)
	
	source = SourceFile(args[1])
	
	try:
		tokens = tokenize(source)
		module = parse(source, tokens)
		analyze(module)
		generateIR(module)
		asm = generateAsm(module)
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
	
	if len(args) == 3:
		outfile = open(args[2], 'w')
		outfile.write(asm.replace('\t', '    '))
	else:
		print(asm.replace('\t', '    '))

if __name__ == '__main__':
	main(sys.argv)
