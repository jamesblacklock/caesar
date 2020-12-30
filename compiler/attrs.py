from .ast.valueref      import ValueRef
from .ast.primitive     import IntLit, BoolLit
from .ast.strlit        import StrLit
from .ast.localdecl     import LetDecl, FnParam
from .ast.fndecl        import FnDecl, CConv
from .ast.structdecl    import StructDecl, FieldDecl
from .symbol.mod        import Mod, TraitDecl
from .log               import logError

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

def acquireDefaultAttr(state, decl, params, span):
	if state.mod.acquireDefaultSet:
		logError(state, span, '`acquire_default` was already set in this scope')
		logExplain(state, state.mod.acquireDefault.span, '`acquire_default` was set here')
		return
	
	state.mod.acquireDefaultSet = True
	state.mod.acquireDefault = decl

def releaseDefaultAttr(state, decl, params, span):
	if state.mod.releaseDefaultSet:
		logError(state, span, '`release_default` was already set in this scope')
		logExplain(state, state.mod.releaseDefault.span, '`release_default` was set here')
		return
	
	state.mod.releaseDefaultSet = True
	state.mod.releaseDefault = decl

def cconvAttr(state, decl, params, span):
	if params[0].value != '"C"':
		logError(state, span, '`cconv` currently only supports the "C" convention')
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

def noStrAttr(state, decl, params, span):
	decl.noStrImport = True

def strModAttr(state, decl, params, span):
	decl.noStrImport = True
	decl.isStrMod = True

def arrModAttr(state, decl, params, span):
	decl.noArrImport = True
	decl.isArrMod = True

def noArrAttr(state, decl, params, span):
	decl.noArrImport = True

def dropTraitAttr(state, decl, params, span):
	decl.isDropTrait = True

def leakAttr(state, decl, params, span):
	decl.leakOwned = True

def inlineAttr(state, decl, params, span):
	if len(params) == 0 or params[0].value:
		decl.alwaysInline = True
	else:
		decl.neverInline = True

AcquireAttr = AttrInfo('acquire_default', acquireDefaultAttr, [FnDecl], [])
ReleaseAttr = AttrInfo('release_default', releaseDefaultAttr, [FnDecl], [])
CConvAttr = AttrInfo('cconv', cconvAttr, [FnDecl], [AttrArg(StrLit)])
AlignAttr = AttrInfo('align', alignAttr, [StructDecl, FieldDecl], [AttrArg(IntLit)])
DropAttr = AttrInfo('drop', dropAttr, [LetDecl, FnParam], [AttrArg(ValueRef)])
NoStrAttr = AttrInfo('no_str', noStrAttr, [Mod], [])
StrModAttr = AttrInfo('str_mod', strModAttr, [Mod], [])
NoArrAttr = AttrInfo('no_arr', noArrAttr, [Mod], [])
ArrModAttr = AttrInfo('arr_mod', arrModAttr, [Mod], [])
DropTraitAttr = AttrInfo('drop_trait', dropTraitAttr, [TraitDecl], [])
LeakAttr = AttrInfo('leak', leakAttr, [ValueRef], [])
InlineAttr = AttrInfo('inline', inlineAttr, [FnDecl], [AttrArg(BoolLit, optional=True)])

builtinAttrs = {
	AcquireAttr.name: AcquireAttr,
	ReleaseAttr.name: ReleaseAttr,
	CConvAttr.name: CConvAttr,
	AlignAttr.name: AlignAttr,
	DropAttr.name: DropAttr,
	NoStrAttr.name: NoStrAttr,
	StrModAttr.name: StrModAttr,
	NoArrAttr.name: NoArrAttr,
	ArrModAttr.name: ArrModAttr,
	DropTraitAttr.name: DropTraitAttr,
	LeakAttr.name: LeakAttr,
	InlineAttr.name: InlineAttr,
}

def invokeAttrs(state, expr):
	if expr.attrsInvoked or expr.attrs == None:
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
