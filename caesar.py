#! /usr/local/bin/python3

import sys
import os
import argparse
import uuid

from compiler.sourcefile                   import SourceFile
from compiler.tokenizer                    import tokenize
from compiler.parser                       import parse
from compiler.analyzer                     import analyze
from compiler.ir                           import generateIR
from compiler.amd64                        import generateAsm

def main(args):
	parser = argparse.ArgumentParser(description='Compiler for the Caesar programming language')
	parser.add_argument(
		'--repl',                                              
		action='store_true',
		default=False,
		help='start the compiler as a REPL interpreter')
	parser.add_argument(
		'--bin',
		'-b',
		nargs='?',
		metavar='FILE_NAME',
		action='store',
		default=False,
		help='name of binary to emit')
	parser.add_argument(
		'--dylib',
		'-d',
		nargs='?',
		metavar='FILE_NAME',
		action='store',
		default=False,
		help='name of dynamic lib to emit')
	parser.add_argument(
		'--lib',
		'-l',
		nargs='?',
		metavar='FILE_NAME',
		action='store',
		default=False,
		help='name of static lib to emit')
	parser.add_argument(
		'--obj',
		'-o',
		action='store_true',
		default=False,
		help='generate object file(s) as output')
	parser.add_argument(
		'--asm',
		'-s',
		nargs='*',
		metavar='INPUT_FILE_NAME', 
		action='store',
		default=False,
		help='generate assembly file(s) as output')
	parser.add_argument(
		'--run',
		'-r',
		nargs='*',
		metavar='PROGRAM_ARGS',
		action='store',
		default=False,
		help='execute the program immediately after compilation')
	parser.add_argument(
		'--sources',
		'-f',
		nargs='*',
		metavar='INPUT_FILE_NAME', 
		action='store',
		default=[], 
		help='file names to compile')
	parser.add_argument(
		'moreSources',
		nargs='*',
		metavar='INPUT_FILE_NAME', 
		action='store',
		default=[], 
		help='file names to compile')
	
	args = parser.parse_args(args[1:])
	
	if args.asm != False:
		args.sources.extend(args.asm)
		args.asm = True
	
	args.sources.extend(args.moreSources)
	
	if not args.sources:
		if args.run != False:
			args.sources = args.run[:1]
			args.run     = args.run[1:]
		elif args.bin != False:
			args.sources = args.bin and [args.bin]
			args.bin     = None
		elif args.lib != False:
			args.sources = args.lib and [args.lib]
			args.lib     = None
		elif args.dylib != False:
			args.sources = args.dylib and [args.dylib]
			args.dylib   = None
	
	binFileName = None
	if args.bin != False:
		if args.bin == None:
			if len(args.sources) != 1:
				print('Error: cannot infer output file name for binary. ' + 
					'Please provide a file name as an argument to `--bin`')
				exit(1)
			args.bin = os.path.splitext(args.sources[0])[0]
		binFileName = args.bin
		args.bin = True
	else:
		binFileName = '/tmp/caesar_tmp_{}'.format(str(uuid.uuid1()))
	
	libFileName = None
	if args.lib != False:
		if args.lib == None:
			if len(args.sources) != 1:
				print('Error: cannot infer output file name for library. ' + 
					'Please provide a file name as an argument to `--lib`')
				exit(1)
			args.lib = os.path.splitext(args.sources[0])[0] + '.lib'
		libFileName = args.lib
		args.lib = True
	
	dyLibFileName = None
	if args.dylib != False:
		if args.dylib == None:
			if len(args.sources) != 1:
				print('Error: cannot infer output file name for DLL. ' + 
					'Please provide a file name as an argument to `--dylib`')
				exit(1)
			args.dylib = os.path.splitext(args.sources[0])[0] + '.dylib'
		dylibFileName = args.dylib
		args.dylib = True
	
	asmFileNames = None
	if args.asm != False:
		asmFileNames = [os.path.splitext(f)[0] + '.asm' for f in args.sources]
	else:
		asmFileNames = ['/tmp/caesar_tmp_{}.asm'.format(str(uuid.uuid1())) for _ in args.sources]
	
	objFileNames = [os.path.splitext(f)[0] + '.o' for f in args.sources]
	
	runArgs = args.run or []
	args.run = not args.run == False
	
	if args.repl:
		if args.run or args.bin or args.dylib or args.lib or args.obj or args.asm:
			print('Error: the following arguments may not be used in REPL mode:')
			print('  --run, --bin, --dylib, --lib, --obj, --asm')
			exit(1)
		elif args.sources:
			print('Error: input file names are not allowed in REPL mode')
			exit(1)
	elif not (args.run or args.bin or args.dylib or args.lib or args.obj or args.asm):
			print('Error: no output method selected. Expected one or more of:')
			print('  --run, --bin, --dylib, --lib, --obj, --asm')
			exit(1)
	elif args.run + args.bin + args.dylib + args.lib > 1:
			print('Error: can only select one of:')
			print('  --run, --bin, --dylib, --lib')
			exit(1)
	elif not args.sources:
		print('Error: no input files supplied')
		exit(1)
	
	if args.repl:
		print('Error: REPL option not yet implemented')
		exit(1)
	elif args.lib or args.dylib:
		print('Error: output methods `--lib` and `--dylib` are currently unimplemented')
		exit(1)
	
	for (i, fileName) in enumerate(args.sources):
		try:
			source = SourceFile(fileName)
		except Exception as e:
			print(e)
			exit(1)
		
		tokens = tokenize(source)
		module = parse(source, tokens)
		analyze(module)
		generateIR(module)
		asm = generateAsm(module)
		
		try:
			outfile = open(asmFileNames[i], 'w')
			outfile.write(asm)
			outfile.close()
			
			if args.obj or args.bin or args.lib or args.dylib or args.run:
				os.system('nasm -f macho64 {} -o {}'.format(asmFileNames[i], objFileNames[i]))
		except Exception as e:
			print(e)
			exit(1)
	
	try:
		if not args.asm and (args.obj or args.bin or args.lib or args.dylib or args.run):
			for f in asmFileNames:
				os.remove(f)
		
		if args.bin or args.run:
			os.system('ld -e _start -macosx_version_min 10.8 -arch x86_64 {} -lc -lSystem -no_pie -o {}'
				.format(' '.join(objFileNames), binFileName))
		
		if args.run and not args.obj:
			for f in objFileNames:
				os.remove(f)
		
		if args.run:
			os.system('{} {}'.format(binFileName, ' '.join(runArgs)))
			os.remove(binFileName)
	except Exception as e:
		print(e)
		exit(1)

if __name__ == '__main__':
	main(sys.argv)
