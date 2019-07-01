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
	parser.add_argument('--repl',                                              action='store_true', default=False, help='start the compiler as a REPL interpreter')
	parser.add_argument('--bin',   '-b', nargs='?', metavar='FILE_NAME',       action='store',      default=False, help='name of binary to emit')
	parser.add_argument('--dylib', '-d', nargs='?', metavar='FILE_NAME',       action='store',      default=False, help='name of dynamic lib to emit')
	parser.add_argument('--lib',   '-l', nargs='?', metavar='FILE_NAME',       action='store',      default=False, help='name of static lib to emit')
	parser.add_argument('--obj',   '-o', nargs='*', metavar='FILE_NAME',       action='store',      default=False, help='generate object file(s) as output')
	parser.add_argument('--asm',   '-s', nargs='*', metavar='FILE_NAME',       action='store',      default=False, help='generate assembly file(s) as output')
	parser.add_argument('--run',   '-r', nargs='*', metavar='PROGRAM_ARGS',    action='store',      default=False, help='execute the program immediately after compilation')
	parser.add_argument('input',         nargs='*', metavar='INPUT_FILE_NAME', action='store',      default=None,  help='file names to compile')
	
	args = parser.parse_args(args[1:])
	
	if not args.input and args.run != False and not (args.bin or args.dylib or args.lib or args.obj or args.asm):
		args.input = args.run[:1]
		args.run = args.run[1:]
	
	binFileName = None
	if args.bin != False:
		if args.bin == None:
			if len(args.input) != 1:
				print('Error: cannot infer output file name for binary. Please provide a file name as an argument to `--bin`')
				exit(1)
			args.bin = os.path.splitext(args.input[0])[0]
		binFileName = args.bin
		args.bin = True
	else:
		binFileName = '/tmp/caesar_tmp_{}'.format(str(uuid.uuid1()))
	
	libFileName = None
	if args.lib != False:
		if args.lib == None:
			if len(args.input) != 1:
				print('Error: cannot infer output file name for library. Please provide a file name as an argument to `--lib`')
				exit(1)
			args.lib = os.path.splitext(args.input[0])[0] + '.lib'
		libFileName = args.lib
		args.lib = True
	else:
		libFileName = '/tmp/caesar_tmp_{}.lib'.format(str(uuid.uuid1()))
	
	dyLibFileName = None
	if args.dylib != False:
		if args.dylib == None:
			if len(args.input) != 1:
				print('Error: cannot infer output file name for DLL. Please provide a file name as an argument to `--dylib`')
				exit(1)
			args.dylib = os.path.splitext(args.input[0])[0] + '.dylib'
		dylibFileName = args.dylib
		args.dylib = True
	else:
		dylibFileName = '/tmp/caesar_tmp_{}.dylib'.format(str(uuid.uuid1()))
	
	asmFileNames = None
	if args.asm != False:
		if len(args.asm) > len(args.input):
			print('Error: found too many arguments to `--asm` (expected up to {}, found {})'
				.format(len(args.input), len(args.asm)))
			exit(1)
		asmFileNames = args.asm
		for f in args.input[len(args.asm):]:
			asmFileNames.append(os.path.splitext(f)[0] + '.asm')
	else:
		asmFileNames = ['/tmp/caesar_tmp_{}.asm'.format(str(uuid.uuid1())) for _ in args.input]
	
	objFileNames = None
	if args.obj != False:
		if len(args.obj) > len(args.input):
			print('Error: found too many arguments to `--obj` (expected up to {}, found {})'
				.format(len(args.input), len(args.obj)))
			exit(1)
		objFileNames = args.obj
		for f in args.input[len(args.obj):]:
			objFileNames.append(os.path.splitext(f)[0] + '.o')
	else:
		objFileNames = ['/tmp/caesar_tmp_{}.o'.format(str(uuid.uuid1())) for _ in args.input]
	
	runArgs = args.run or []
	args.run = not args.run == False
	
	if args.repl:
		if args.run or args.bin or args.dylib or args.lib or args.obj or args.asm:
			print('Error: the following arguments may not be used in REPL mode:')
			print('\t--run, --bin, --dylib, --lib, --obj, --asm')
			exit(1)
		elif args.input:
			print('Error: input file names are not allowed in REPL mode')
			exit(1)
	elif not (args.run or args.bin or args.dylib or args.lib or args.obj or args.asm):
			print('Error: no output method selected. Expected one or more of:')
			print('\t--run, --bin, --dylib, --lib, --obj, --asm')
			exit(1)
	elif not args.input:
		print('Error: no input files supplied')
		exit(1)
	
	if args.repl:
		print('Error: REPL option not yet implemented')
		exit(1)
	elif args.lib or args.dylib:
		print('Error: output methods `--lib` and `--dylib` are currently unimplemented')
		exit(1)
	
	for (i, fileName) in enumerate(args.input):
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
		if args.bin or args.run:
			os.system('ld -e _start -macosx_version_min 10.8 -arch x86_64 {} -lc -lSystem -no_pie -o {}'.format(
				' '.join(objFileNames), binFileName))
		
		if args.run:
			os.system('{} {}'.format(binFileName, ' '.join(runArgs)))
		
		if not args.asm and (args.obj or args.bin or args.lib or args.dylib or args.run):
			for f in asmFileNames:
				os.remove(f)
	except Exception as e:
		print(e)
		exit(1)

if __name__ == '__main__':
	main(sys.argv)
