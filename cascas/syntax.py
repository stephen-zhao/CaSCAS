from shared import Shared


# SyntaxNode is the base class for all node types that are allowed in a
#   syntax tree
# Grammar def:
# SyntaxNode = FuncNode
#            | AddNode
#            | SubNode
#            | MultNode
#            | DivNode
#            | ExpNode
#            | QNode
#            | SymNode
#            | SyntaxTree (temporary)
class SyntaxNode:

    def __init__(self):
        # field for keyword to identify type of SyntaxNode
        self.keyword = ""
        # field for node count of subtree at self
        self.nodeCount = 0
        # field for a generic list of node parameters,
        #   usually used for subnodes (lsn = list sub
        #   nodes)
        self.lsn = []
        # field as reference to parent node
        self.parent = None

    def __repr__(self):
        return (
            "<" + type(self).__name__ + " " + self.keyword + " " + " ".join(
                map(lambda n: repr(n), self.lsn)
            ) + ">"
        )

    def __str__(self):
        if not self.lsn:
            return self.keyword
        else:
            return (
                "(" + self.keyword + " " + " ".join(
                    map(lambda n: str(n), self.lsn)
                ) + ")"
            )

    def initAsParent(parent):
        for child in parent.lsn:
            child.parent = parent
            parent.nodeCount += child.nodeCount
        parent.nodeCount += 1

    def print(self):
        print(str(self))
        return self

    def isExplicitlyZero(self):
        return False

    def isExplicitlyOne(self):
        return False

    # getUniqueSubNodeIndexThatIs
    # Gets the index of the only child node that is (pred)
    # Returns index if such a child exists & all other children are not (pred)
    # Returns -2 if more than one child is (pred)
    # Returns -1 if no children are (pred)
    def getUniqueSubNodeIndexThatIs(self, pred):
        iChildThatIsPred = -1
        for i in range(len(self.lsn)):
            if pred(self.lsn[i]):
                if iChildThatIsPred == -1:
                    iChildThatIsPred = i
                else:
                    iChildThatIsPred = -2
        return iChildThatIsPred

    def isOneOrLessSubNodes(self, pred):
        encounteredOne = False
        for sn in self.lsn:
            if pred(sn):
                if not encounteredOne:
                    encounteredOne = True
                else:
                    return False
        return True

    def setRecountNodes(self):
        self.nodeCount = sum(map(lambda sn: sn.nodeCount, self.lsn), 1)

    def simplify(self):
        return self.getRoot().simplifyTrivials(0).parent

    def simplifyTrivials__subnodes(self, iChild):
        for i in range(len(self.lsn)):
            self.lsn[i].simplifyTrivials(i)

    def simplifyTrivials__after(self, iChild):
        self.setRecountNodes()
        self.parent.lsn[iChild].parent = self.parent

    # SyntaxNode -> SyntaxNode
    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)
        self.simplifyTrivials__after(iChild)
        return self


# SyntaxTree wraps the root node of a syntax tree made of nested SyntaxNodes
class SyntaxTree(SyntaxNode):
    KEYWORD = "Tree"

    def __init__(self, root):
        super().__init__()
        self.keyword = SyntaxTree.KEYWORD
        self.lsn = [root]
        super().initAsParent()

    def __str__(self):
        return str(self.getRoot())

    def getRoot(self):
        return self.lsn[0]

    def print(self):
        if Shared.g_optionFlags & Shared.FLG_DEBUG:
            print("Syntax tree:", str(self.getRoot()))
            print("Node count:", self.getRoot().nodeCount)
        else:
            print(str(self))
        return self


# FuncNode represents a function application node in a syntax tree
class FuncNode(SyntaxNode):
    def __init__(self, name, *args):
        super().__init__()
        self.keyword = name
        self.lsn = list(args)
        super().initAsParent()

    def name(self):
        return self.lsn[0]

    def args(self):
        return self.lsn[1:]

    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)
        self.simplifyTrivials__after(iChild)
        return self


# AddNode represents an addition node in a syntax tree
class AddNode(SyntaxNode):
    KEYWORD = "+"

    def __init__(self, *addends):
        super().__init__()
        self.keyword = AddNode.KEYWORD
        self.lsn = list(addends)
        super().initAsParent()

    def addends(self):
        return self.lsn

    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)
        if (self.isOneOrLessSubNodes(
                lambda sn: not sn.isExplicitlyZero())):
            subNodeIndex = self.getUniqueSubNodeIndexThatIs(
                lambda sn: not sn.isExplicitlyZero()
            )
            self.parent.lsn[iChild] = self.addends()[
                subNodeIndex if subNodeIndex != -1 else 0
            ]

        self.simplifyTrivials__after(iChild)
        return self


# SubNode represents a binary subtraction node in a syntax tree
class SubNode(SyntaxNode):
    KEYWORD = "-"

    def __init__(self, minuend, subtractend):
        super().__init__()
        self.keyword = SubNode.KEYWORD
        self.lsn = [minuend, subtractend]
        super().initAsParent()

    def minuend(self):
        return self.lsn[0]

    def subtractend(self):
        return self.lsn[1]

    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)
        if self.subtractend().isExplicitlyZero():
            self.parent.lsn[iChild] = self.minuend()

        self.simplifyTrivials__after(iChild)
        return self


# MultNode represents a multiplication node in a syntax tree
class MultNode(SyntaxNode):
    KEYWORD = "*"

    def __init__(self, *multiplicands):
        super().__init__()
        self.keyword = MultNode.KEYWORD
        self.lsn = list(multiplicands)
        super().initAsParent()

    def multiplicands(self):
        return self.lsn

    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)
        if any(map(lambda sn: sn.isExplicitlyZero(), self.multiplicands())):
            self.parent.lsn[iChild] = QNode(0)

        elif (self.isOneOrLessSubNodes(
                lambda sn: not sn.isExplicitlyOne())):
            subNodeIndex = self.getUniqueSubNodeIndexThatIs(
                lambda sn: not sn.isExplicitlyOne()
            )
            self.parent.lsn[iChild] = self.multiplicands()[
                subNodeIndex if subNodeIndex != -1 else 0
            ]

        # TODO: add trivial simplification for negative QNodes

        self.simplifyTrivials__after(iChild)
        return self


# DivNode represents a binary division node in a syntax tree
class DivNode(SyntaxNode):
    KEYWORD = "/"

    def __init__(self, dividend, divisor):
        super().__init__()
        self.keyword = DivNode.KEYWORD
        self.lsn = [dividend, divisor]
        super().initAsParent()

    def dividend(self):
        return self.lsn[0]

    def divisor(self):
        return self.lsn[1]

    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)

        if self.divisor().isExplicitlyOne():
            self.parent.lsn[iChild] = self.dividend()

        elif self.divisor().isExplicitlyZero():
            print("fuqu")

        self.simplifyTrivials__after(iChild)
        return self


# ExpNode represents a binary exponentiation node in a syntax tree
class ExpNode(SyntaxNode):
    KEYWORD = (
        "expt" if Shared.g_optionFlags &
        Shared.FLG_LANG_RACKET else "^"
    )

    def __init__(self, base, exponent):
        super().__init__()
        self.keyword = ExpNode.KEYWORD
        self.lsn = [base, exponent]
        super().initAsParent()

    def base(self):
        return self.lsn[0]

    def exponent(self):
        return self.lsn[1]

    def simplifyTrivials(self, iChild):
        self.simplifyTrivials__subnodes(iChild)
        if (self.exponent().isExplicitlyZero() and
                self.base().isExplicitlyZero()):
            print("tbd")

        elif self.exponent().isExplicitlyZero():
            self.parent.lsn[iChild] = QNode(1)

        elif self.base().isExplicitlyZero():
            self.parent.lsn[iChild] = QNode(0)

        elif self.exponent().isExplicitlyOne():
            self.parent.lsn[iChild] = self.base()

        self.simplifyTrivials__after(iChild)
        return self


# QNode represents a rational number in a syntax tree
class QNode(SyntaxNode):
    KEYWORD = "QNode"

    def __init__(self, primitiveValue):
        super().__init__()
        self.keyword = QNode.KEYWORD
        if type(primitiveValue) is int:
            self.lsn = [primitiveValue, 1]
        elif type(primitiveValue) is float:
            self.lsn = list(primitiveValue.as_integer_ratio())
        elif type(primitiveValue) is str:
            if primitiveValue.find(".") == -1:
                self.lsn = [int(primitiveValue), 1]
            else:
                self.lsn = self.getNumerOverDenom(primitiveValue)
        else:
            self.lsn = [int(primitiveValue), 1]
        self.nodeCount = 1

    def getNumerOverDenom(self, s):
        iDot = s.find(".")
        numOfTens = len(s) - iDot - 1
        denom = 10**numOfTens
        ls = list(s)
        ls.remove('.')
        numer = int(''.join(ls))
        return [numer, denom]

    def approxVal(self):
        return (1.0 * self.lsn[0]) / (1.0 * self.lsn[1])

    def numerator(self):
        return self.lsn[0]

    def denominator(self):
        return self.lsn[1]

    def simplifyTrivials(self, iChild):
        return self

    def isExplicitlyZero(self):
        return self.numerator() == 0

    def isExplicitlyOne(self):
        return self.numerator() == self.denominator()

    def sign(self):
        if self.numerator() == 0:
            return 0
        elif (self.numerator() > 0) == (self.denominator() > 0):
            return 1
        else:
            return -1

    def __str__(self):
        if self.isExplicitlyZero():
            return "0"
        elif self.denominator() == 1:
            return str(self.numerator())
        else:
            return str(self.numerator()) + "/" + str(self.denominator())


# SymNode represents a symbol (variable or constant or ) in a syntax tree
class SymNode(SyntaxNode):
    def __init__(self, symbol):
        super().__init__()
        self.keyword = symbol
        self.lsn = []
        self.nodeCount = 1

    def simplifyTrivials(self, iChild):
        return self
