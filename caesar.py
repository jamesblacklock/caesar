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
	try:
		if len(args) < 2 or len(args) > 3:
			print('Usage: caesar <input filename> [<output filename>]')
			exit(1)
		
		source = SourceFile(args[1])
		tokens = tokenize(source)
		module = parse(source, tokens)
		analyze(module)
		generateIR(module)
		asm = generateAsm(module)
		
		if len(args) == 3:
			outfile = open(args[2], 'w')
			outfile.write(asm)
			outfile.close()
		else:
			print(asm.replace('\t', '    '))
		
	except Exception as e:
		print(e)

if __name__ == '__main__':
	main(sys.argv)
