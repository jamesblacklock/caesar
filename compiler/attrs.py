from .ast        import AST
from .valueref   import ValueRef
from .primitive  import IntLit, StrLit
from .structdecl import StructDecl, FieldDecl
from .fndecl     import FnDecl, CConv
from .letdecl    import LetDecl, FnParam
from .log        import logError

class Attr(AST):
	def __init__(self, name, args, span):
		super().__init__(span)
		self.name = name
		self.args = args
	
	def pretty(self, output, indent=0):
		output.write('@', indent)
		output.write(self.name)
		if len(self.args) > 0:
			output.write('(')
			self.args[0].pretty(output)
			for arg in self.args[1:]:
				output.write(', ')
				arg.pretty(output)
			output.write(')')

def acquireDefaultAttr(state, decl, params, span):
	if state.scope.acquireDefaultSet:
		logError(state, span, '`acquire_default` was already set in this scope')
		logExplain(state, state.scope.acquireDefault.span, '`acquire_default` was set here')
		return
	
	state.scope.acquireDefaultSet = True
	state.scope.acquireDefault = decl

def releaseDefaultAttr(state, decl, params, span):
	if state.scope.releaseDefaultSet:
		logError(state, span, '`release_default` was already set in this scope')
		logExplain(state, state.scope.releaseDefault.span, '`release_default` was set here')
		return
	
	state.scope.releaseDefaultSet = True
	state.scope.releaseDefault = decl

def ffiAttr(state, decl, params, span):
	if params[0].value != 'C':
		logError(state, span, '`ffi` currently only supports the "C" convention')
	else:
		decl.cconv = CConv.C

def alignAttr(state, decl, params, span):
	align = params[0].value
	if align < 1 or (align & (align - 1)) != 0:
		logError(state, span, 'alignment must be a power of 2 greater than or equal to 1')
	else:
		decl.align = align

def dropAttr(state, decl, params, span):
	decl.dropFn = state.lookupSymbol(params[0].path, inValuePosition=True)

def cstrAttr(state, decl, params, span):
	decl.cstr = True

class AttrInfo:
	def __init__(self, name, proc, appliesTo, argInfo):
		self.name = name
		self.proc = proc
		self.appliesTo = appliesTo
		self.argInfo = argInfo

class AttrArg:
	def __init__(self, types, optional=False):
		self.types = types if type(types) == list else [types]
		self.optional = optional

AcquireAttr = AttrInfo('acquire_default', acquireDefaultAttr, [FnDecl], [])
ReleaseAttr = AttrInfo('release_default', releaseDefaultAttr, [FnDecl], [])
FFIAttr = AttrInfo('ffi', ffiAttr, [FnDecl], [AttrArg(StrLit)])
AlignAttr = AttrInfo('align', alignAttr, [StructDecl, FieldDecl], [AttrArg(IntLit)])
DropAttr = AttrInfo('drop', dropAttr, [LetDecl, FnParam], [AttrArg(ValueRef)])
CStrAttr = AttrInfo('cstr', cstrAttr, [StrLit], [])

builtinAttrs = {
	AcquireAttr.name: AcquireAttr, 
	ReleaseAttr.name: ReleaseAttr, 
	FFIAttr.name: FFIAttr, 
	AlignAttr.name: AlignAttr, 
	DropAttr.name: DropAttr, 
	CStrAttr.name: CStrAttr
}

def invokeAttrs(state, expr):
	if expr.attrs == None or expr.attrsInvoked:
		return
	
	expr.attrsInvoked = True
	
	for attr in expr.attrs:
		if attr.name not in builtinAttrs:
			logError(state, attr.span, 'unrecognized attribute: `{}`'.format(attr.name))
			continue
		
		attrInfo = builtinAttrs[attr.name]
		
		if type(expr) not in attrInfo.appliesTo:
			logError(state, attr.span, 
				'the `{}` attribute cannot be applied to this expression'.format(attr.name))
			continue
		
		if len(attr.args) > len(attrInfo.argInfo):
			logError(state, attr.span, '`{}` takes {} arguments (found {})'.format(
				attr.name, len(attrInfo.argInfo), len(attr.args)))
			continue
		
		if len(attr.args) < len(attrInfo.argInfo):
			invalidArgs = False
			for info in attrInfo.argInfo[len(attr.args):]:
				if not info.optional:
					logError(state, attr.span, '`{}` takes {} argument{} (found {})'.format(
						attr.name, len(attrInfo.argInfo), 
						'' if len(attrInfo.argInfo) == 1 else 's', len(attr.args)))
					invalidArgs = True
					break
			if invalidArgs:
				continue
		
		invalidArgs = False
		for (arg, info) in zip(attr.args, attrInfo.argInfo):
			if type(arg) not in info.types:
				logError(state, arg.span, '`{}`: invalid argument'.format(attr.name))
				invalidArgs = True
		
		if invalidArgs:
			continue
		
		attrInfo.proc(state, expr, attr.args, attr.span)
