from .symbol   import Symbol, SymbolType
from ..ast.ast import Name
from ..types   import Type

class Trait(Symbol):
	def __init__(self, ast):
		from .mod import Mod
		super().__init__(SymbolType.TYPE, ast.name, ast.nameSpan, ast.span, ast.pub)
		modName = Name('$traitmod__' + self.name, self.nameSpan)
		self.ast = ast
		self.mod = Mod(modName, None, ast.decls, self.span)
		self.type = Type(self.name, span=self.span, isTraitType=True, symbol=self)
		self.type.isDropTrait = False
		self.type.mod = self.mod
		self.type.symbolTable = self.mod.symbolTable
	
	def checkSig(self, state):
		self.type.isDropTrait = self.ast.isDropTrait
		# self.symbolTable = self.mod.symbolTable
		self.type.mod.checkSig(state)
	
	def analyze(self, state, deps):
		if self in deps:
			return
		
		deps.push(self)
		self.type.mod.analyze(state, deps)
		deps.pop()