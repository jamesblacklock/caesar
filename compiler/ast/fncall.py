from .ast               import AST
from .                  import block, valueref, primitive
from ..not_done         import enumdecl
from ..mir              import access as accessmod
from .structlit         import StructLit, FieldLit
from .tuplelit          import TupleLit
from ..types            import PtrType, TypeSymbol, Void
from ..log              import logError, logWarning
from ..mir.fncall       import FnCall as FnCallMIR
from ..mir.createstruct import CreateStruct
from ..mir.primitive    import IntValue
from ..mir.access       import SymbolAccess
from .address           import Address

class FnCall(AST):
	def __init__(self, expr, args, span):
		super().__init__(span, True)
		self.isMethodCall = False
		self.expr = expr
		self.args = args
	
	def analyze(self, state, implicitType):
		access = None
		dynDispatch = False
		selfArg = None
		if self.isMethodCall:
			selfArg = SymbolAccess.analyzeSymbolAccess(state, self.args[0])
			if selfArg.type:
				name = self.expr.path[0].content
				symbol = None
				if name in selfArg.type.symbolTable:
					symbol = selfArg.type.symbolTable[name]
				if selfArg.type.extern and not symbol.pub:
					symbol = None
				if symbol == None:
					logError(state, self.expr.span, 'cannot resolve the method `{}` for type `{}`'.format(name, selfArg.type))
				else:
					if selfArg.type.isTraitType:
						dynDispatch = True
					access = accessmod.SymbolRead(self.expr.span)
					access.symbol = symbol
					access.type = symbol.type
		elif type(self.expr) == valueref.ValueRef:
			variants = implicitType.symbolTable if implicitType and implicitType.isEnumType else None
			symbol = state.lookupSymbol(self.expr.path, variants)
			if symbol:
				enumType = None
				variant = None
				if type(symbol) == enumdecl.VariantDecl and symbol.type:
					enumType = symbol.enumType
					variant = symbol
					symbol = symbol.type
				if isinstance(symbol, TypeSymbol):
					if symbol.isTupleType:
						data = TupleLit(self.args, self.span)
						return CreateStruct.create(state, enumType, self.span, [
							CreateStruct.initStruct('$data', [
								CreateStruct.init('$' + variant.name, data)
							]),
							CreateStruct.init('$tag', IntValue(variant.tag.data, enumType.tagType, self.span))
						])
					else:
						logError(state, self.expr.span, 'the expression cannot be called as a function')
				else:
					access = accessmod.SymbolRead(self.expr.span)
					access.symbol = symbol
					access.type = symbol.type
		else:
			access = state.analyzeNode(self.expr, implicitType)
		
		fnType = None
		if access and access.type:
			if not access.type.isFnType:
				logError(state, self.expr.span, 'the expression cannot be called as a function')
			else:
				fnType = access.type
				if self.isMethodCall:
					if len(fnType.params) > 0 and fnType.params[0].type.isPtrType:
						selfArg = Address(self.args[0], fnType.params[0].type.mut, self.args[0].span)
						selfArg = state.analyzeNode(selfArg)
				
				if fnType.unsafe and not state.scope.allowUnsafe:
					logError(state, self.expr.span, 'unsafe function called in a safe context')
				
				if len(self.args) < len(fnType.params) or \
					not fnType.cVarArgs and len(self.args) > len(fnType.params):
					logError(state, self.span, 
						'function called with wrong number of arguments (expected {}, found {})'
							.format(len(fnType.params), len(self.args)))
		
		params = list(fnType.params) if fnType else []
		for _ in range(len(params), len(self.args)):
			params.append(None)
		
		isSelfArg = self.isMethodCall
		hasCVarArgs = fnType.cVarArgs if fnType else False
		args = []
		argFailed = False
		for (param, arg) in zip(params, self.args):
			expectedType = param.type if param else None
			
			if isSelfArg:
				arg = selfArg
			else:
				arg = state.analyzeNode(arg, expectedType)
				if arg and arg.type and not arg.type.isPrimitiveType and not arg.type.isUnknown:
					if param == None and hasCVarArgs:
						logError(state, arg.span, 'type {} cannot be used as a C variadic argument'.format(arg.type))
			
			if arg == None:
				argFailed = True
				continue
			
			if arg.type and arg.type.isPtrType and arg.typeModifiers and arg.typeModifiers.uninit:
				if param:
					logError(state, arg.span, 'reference to uninit symbol passed as parameter `{}`'.format(param.name))
				else:
					assert cVarArgs
					logError(state, arg.span, 'reference to uninit symbol passed as C variadic argument')
			
			arg = state.typeCheck(arg, expectedType)
			args.append(arg)
			isSelfArg = False
		
		cVarArgs = []
		if hasCVarArgs:
			cVarArgs = args[len(fnType.params):]
			args = args[:len(fnType.params)]
		
		returnType = fnType.returnType if fnType else Void
		if access == None or argFailed:
			mir = None
		else:
			mir = FnCallMIR(access, args, cVarArgs, dynDispatch, returnType, self.span)
		
		for arg in args:
			if arg.borrows:
				lateRef = SymbolAccess.noop(arg.symbol, state.scope.dropBlock, arg.span)
				state.scope.dropBlock.append(lateRef)
		
		return mir
