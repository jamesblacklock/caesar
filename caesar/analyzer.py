from caesar.exception import CsrCompileError
from caesar.parser import FnDeclAST, CConv

class Mod:
	def __init__(self, symbolTable):
		self.symbolTable = symbolTable

def ffiAttr(decl, params):
	if len(params) != 1 or params[0].content != '"C"':
		raise CsrCompileError('FFI currently only supports the "C" convention')
	if type(decl) != FnDeclAST:
		raise CsrCompileError('FFI attribute can only be applied to functions')
	
	decl.cconv = CConv.C
	decl.mangledName = '_{}'.format(decl.name)

builtinAttrs = \
{
	'FFI': ffiAttr
}

def invokeAttrs(decl):
	for attr in decl.attrs:
		attrHandler = builtinAttrs.get(attr.name)
		if attrHandler == None:
			raise CsrCompileError('attribute "{}" does not exist'.format(attr.name))
		
		attrHandler(decl, attr.args)

# def generate(mod):
# 	pack = Pack()
# 	state = GeneratorState(mod, pack)
	
# 	for decl in mod.symbolTable.values():
# 		meta = invokeAttrs(decl, state)

def analyze(mod):
	for decls in (mod.importDecls, mod.staticDecls, mod.fnDecls):
		for decl in decls:
			invokeAttrs(decl)
	
	return mod
