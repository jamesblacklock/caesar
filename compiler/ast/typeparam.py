from .ast import AST

class TypeParam(AST):
    def __init__(self, name, valueType, span):
        super().__init__(span)
        self.name = name
        self.valueType = valueType