import os
from . import platform

def buildObjFile(asmFile, objFile):
	if platform.MacOS:
		objFormat = 'macho64'
	elif platform.Linux:
		objFormat = 'elf64'
	else:
		print('assembly not yet implemented for Windows')
		exit(1)
	
	asmExitCode = os.system('nasm -f {} {} -o {}'.format(objFormat, asmFile, objFile))
	return asmExitCode == 0