import regex

LANG_RACKET = True


class SyntaxNode:
    # | FuncNode
    # | AddNode
    # | SubNode
    # | MultNode
    # | DivNode
    # | ExpNode
    # | QNode
    # | SymNode
    # | SyntaxTree

    STD_DET = 0
    DBG_DET = 1

    def __init__(self):
        self.keyword = ""   # keyword to identify it
        self.lsn = []       # list of sub-nodes
        self.nodeCount = 0
        self.parent = None

    def __repr__(self):
        if not self.lsn:
            return self.keyword
        else:
            return "(" + self.keyword + " " + " ".join(map(lambda n: n.__repr__(), self.lsn)) + ")"

    def __str__(self):
        if not self.lsn:
            return self.keyword
        else:
            return "(" + self.keyword + " " + " ".join(map(lambda n: n.__str__(), self.lsn)) + ")"

    def initAsParent(parent):
        for child in parent.lsn:
            child.parent = parent
            parent.nodeCount += child.nodeCount
        parent.nodeCount += 1

    def printSyntaxTree(self, detail=STD_DET):
        if detail == SyntaxNode.DBG_DET:
            print(self.__repr__())
            print(self.nodeCount)
        else:
            print(self.__str__())
        return self

    def isZero(self):
        if (type(self) is QNode):
            return self.numerator() == 0
        else:
            return False

    def isOne(self):
        if (type(self) is QNode):
            return self.numerator() == self.denominator()
        else:
            return False

    # getSubNodeIndexThatIsntPred
    # Gets the index of the only child node that isn't (pred)
    # Returns index if such a child exists and all other children are (pred)
    # Returns 0 if all children are (pred)
    # Returns -1 if more than one children are (pred)
    def getSubNodeIndexThatIsntPred(self, pred):
        iChildThatIsntPred = -1
        for i in range(len(self.lsn)):
            if not pred(self.lsn[i]):
                if iChildThatIsntPred == -1:
                    iChildThatIsntPred = i
                else:
                    return -1
        if iChildThatIsntPred == -1:
            return 0
        else:
            return iChildThatIsntPred

    def isOneOrLessSubNodesNotPred(self, pred):
        numNotPred = 0
        for sn in self.lsn:
            if not pred(sn):
                numNotPred += 1
            if numNotPred > 1:
                return False
        return True

    def setRecountNodes(self):
        self.nodeCount = sum(map(lambda sn: sn.nodeCount, self.lsn), 1)

    def simplify(self):
        return self.getRoot().simplifyTrivials(0).parent

    # SyntaxNode -> SyntaxNode
    def simplifyTrivials(self, iChild):
        if (type(self) is QNode or
                type(self) is SymNode):
            return self

        for i in range(len(self.lsn)):
            self.lsn[i].simplifyTrivials(i)

        if (type(self) is ExpNode and
                self.exponent().isZero() and
                self.base().isZero()):
            print("tbd")

        elif (type(self) is ExpNode and
                self.exponent().isZero()):
            self.parent.lsn[iChild] = QNode(1)

        elif (type(self) is ExpNode and
                self.base().isZero()):
            self.parent.lsn[iChild] = QNode(0)

        elif (type(self) is ExpNode and
                self.exponent().isOne()):
            self.parent.lsn[iChild] = self.base()

        elif (type(self) is SubNode and
                self.subtractend().isZero()):
            self.parent.lsn[iChild] = self.minuend()

        elif (type(self) is MultNode and
                any(map(lambda sn: sn.isZero(), self.multiplicands()))):
            self.parent.lsn[iChild] = QNode(0)

        elif (type(self) is MultNode and
                self.isOneOrLessSubNodesNotPred(SyntaxNode.isOne)):
            self.parent.lsn[iChild] = self.multiplicands()[
                self.getSubNodeIndexThatIsntPred(SyntaxNode.isOne)
            ]

        elif (type(self) is AddNode and
                self.isOneOrLessSubNodesNotPred(SyntaxNode.isZero)):
            self.parent.lsn[iChild] = self.addends()[
                self.getSubNodeIndexThatIsntPred(SyntaxNode.isZero)
            ]

        elif (type(self) is DivNode and
                self.divisor().isOne()):
            self.parent.lsn[iChild] = self.dividend()

        elif (type(self) is DivNode and
                self.divisor().isZero()):
            print("fuqu")

        self.setRecountNodes()
        self.parent.lsn[iChild].parent = self.parent

        return self


class SyntaxTree(SyntaxNode):
    KEYWORD = "Tree"

    def __init__(self, root):
        SyntaxNode.__init__(self)
        self.keyword = SyntaxTree.KEYWORD
        self.lsn = [root]
        SyntaxNode.initAsParent(self)

    def getRoot(self):
        return self.lsn[0]

    def printSyntaxTree(self, detail=SyntaxNode.STD_DET):
        if detail == SyntaxNode.DBG_DET:
            print("Syntax tree:", self.getRoot().__repr__())
            print("Node count:", self.getRoot().nodeCount)
        else:
            print(self.__str__())
        return self


class FuncNode(SyntaxNode):
    def __init__(self, name, *args):
        SyntaxNode.__init__(self)
        self.keyword = name
        self.lsn = list(args)
        SyntaxNode.initAsParent(self)

    def name(self):
        return self.lsn[0]

    def args(self):
        return self.lsn[1:]


class AddNode(SyntaxNode):
    KEYWORD = "+"

    def __init__(self, *addends):
        SyntaxNode.__init__(self)
        self.keyword = AddNode.KEYWORD
        self.lsn = list(addends)
        SyntaxNode.initAsParent(self)

    def addends(self):
        return self.lsn


class SubNode(SyntaxNode):
    KEYWORD = "-"

    def __init__(self, minuend, subtractend):
        SyntaxNode.__init__(self)
        self.keyword = SubNode.KEYWORD
        self.lsn = [minuend, subtractend]
        SyntaxNode.initAsParent(self)

    def minuend(self):
        return self.lsn[0]

    def subtractend(self):
        return self.lsn[1]


class MultNode(SyntaxNode):
    KEYWORD = "*"

    def __init__(self, *multiplicands):
        SyntaxNode.__init__(self)
        self.keyword = MultNode.KEYWORD
        self.lsn = list(multiplicands)
        SyntaxNode.initAsParent(self)

    def multiplicands(self):
        return self.lsn


class DivNode(SyntaxNode):
    KEYWORD = "/"

    def __init__(self, dividend, divisor):
        SyntaxNode.__init__(self)
        self.keyword = DivNode.KEYWORD
        self.lsn = [dividend, divisor]
        SyntaxNode.initAsParent(self)

    def dividend(self):
        return self.lsn[0]

    def divisor(self):
        return self.lsn[1]


class ExpNode(SyntaxNode):
    KEYWORD = "expt" if LANG_RACKET else "^"

    def __init__(self, base, exponent):
        SyntaxNode.__init__(self)
        self.keyword = ExpNode.KEYWORD
        self.lsn = [base, exponent]
        SyntaxNode.initAsParent(self)

    def base(self):
        return self.lsn[0]

    def exponent(self):
        return self.lsn[1]


class QNode(SyntaxNode):
    KEYWORD = "QNode"

    def __init__(self, primitiveValue):
        SyntaxNode.__init__(self)
        self.keyword = QNode.KEYWORD
        if type(primitiveValue) is int:
            self.lsn = [primitiveValue, 1]
        elif type(primitiveValue) is float:
            self.lsn = list(primitiveValue.as_integer_ratio())
        else:
            self.lsn = [int(primitiveValue), 1]
        self.nodeCount = 1

    def approxVal(self):
        return (1.0 * self.lsn[0]) / (1.0 * self.lsn[1])

    def numerator(self):
        return self.lsn[0]

    def denominator(self):
        return self.lsn[1]

    def sign(self):
        if self.numerator() == 0:
            return 0
        elif (self.numerator() > 0) == (self.denominator() > 0):
            return 1
        else:
            return -1

    def __repr__(self):
        if self.isZero():
            return "0"
        elif self.denominator() == 1:
            return str(self.numerator())
        else:
            return str(self.numerator()) + "/" + str(self.denominator())

    def __str__(self):
        if self.isZero():
            return "0"
        elif self.denominator() == 1:
            return str(self.numerator())
        else:
            return str(self.numerator()) + "/" + str(self.denominator())


class SymNode(SyntaxNode):
    def __init__(self, symbol):
        SyntaxNode.__init__(self)
        self.keyword = symbol
        self.lsn = []
        self.nodeCount = 1


class Parser:
    def __init__(self):
        SyntaxNode.__init__(self)
        self.TOKENIZING_REGEX_STRING \
            = r"[\+\-\*/\^]"\
            r"|[0-9]+"\
            r"|(?P<brackets>\((?:[^\(\)]|(?0))*\))"\
            r"|([A-Za-z][A-Za-z0-9]*(?P<funcapp>\((?:[^\(\)]|(?0))*\))?)"

        self.LIST_KNOWN_FUNCTION_NAMES \
            = [
                "sin", "cos", "tan", "sec", "csc", "cot",
                "arcsin", "arccos", "arctan", "arcsec", "arccsc", "arccot",
                "log", "ln"
            ]

    # Str, Int, Token? -> Void
    def printTokenizeErr(self, s, i, t=""):
        print(
            (
                "Error tokenizing \"{0}\" at character {1}: '{2}'. " +
                "Current token: \"{3}\""
            )
            .format(s, i, s[i], t)
        )

    # Token -> (Listof Token)
    def tokenize(self, s):
        ci = 0          # current char index in s
        lt = []         # list of created tokens
        while ci < len(s):
            if s[ci] == ' ':
                ci += 1
                continue
            rem = regex.match(self.TOKENIZING_REGEX_STRING, s[ci:])
            if not rem:
                self.printTokenizeErr(s, ci)
                return []
            lt.append(rem.group())
            ci += rem.end()
        return lt

    # Str -> SyntaxNode
    def parse(self, s):
        return SyntaxTree(self.parseTokens(self.tokenize(s)))

    # (listof Token) -> SyntaxNode
    def parseTokens(self, lt):
        # check for base cases
        if len(lt) == 1:
            # check for parenthesized statements (...)
            if lt[0][0] == '(' and lt[0][-1] == ')':
                return self.parseTokens(self.tokenize(lt[0][1:-1]))
            # check for numerical numbers
            if regex.match(r"[0-9]+", lt[0]):
                return QNode(lt[0])
            # check for known functions
            for func in self.LIST_KNOWN_FUNCTION_NAMES:
                if (lt[0].startswith(func) and
                        lt[0][len(func)] == '(' and
                        lt[0][-1] == ')'):
                    return FuncNode(
                        func,
                        self.parseTokens(self.tokenize(lt[0][len(func) + 1:-1]))
                    )
            # otherwise is a symbol of some sort
            return SymNode(lt[0])

        # check for addition
        for ti in range(0, len(lt)):
            # if lt[ti] == "-":
            #     newltfromti = self.subtoplus(lt[ti:])    # new lt elements for elements from ti to end
            #     lt = lt[0:ti] + newltfromti[:]
            if lt[ti] == "+":
                return AddNode(self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:]))
        # check for subtraction
        for ti in range(len(lt) - 1, -1, -1):
            if lt[ti] == "-":
                return SubNode(self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:]))
        # check for multiplication
        for ti in range(0, len(lt)):
            if lt[ti] == "*":
                return MultNode(self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:]))
        # check for division
        for ti in range(len(lt) - 1, -1, -1):
            if lt[ti] == "/":
                return DivNode(self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:]))
        # check for exponentiation
        for ti in range(0, len(lt)):
            if lt[ti] == "^":
                return ExpNode(self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:]))

    # (Listof Token) -> (Listof Token)
    # deprecated
    def subtoplus(self, lt):
        # takes in a list of tokens in the form of ["-", t1, t2, ... tn-1, regex"[+-]", ...]
        # converts it to ["+", "-1", "*", "(t1 t1 t2 ... tn-1)", ...]
        newlt = ["+", "-1", "*"]
        try:
            iFirstAddSub = lt.index("+", 1)
            pass
        except ValueError:
            try:
                iFirstAddSub = lt.index("-", 1)
                pass
            except ValueError:
                iFirstAddSub = None
        if iFirstAddSub is None:
            newlt.append("(" + " ".join(lt[1:]) + ")")
        else:
            newlt.append("(" + " ".join(lt[1:iFirstAddSub]) + ")")
            newlt += lt[iFirstAddSub:]
        return newlt


P = Parser()
# F1 = "sin(4+x)-x^(3-y)+x*y^2/u-6*tan(x)-2*x+y^(x)"
# F = "sin(4+x)"
# print(P.tokenize(F))
# print(P.subtoplus(["-", "4", "*", "x", "/", "y", "^", "(2 + e)"]))
# pF = P.parse(F).printSyntaxTree(1)

F2 = "1 +0"
F3 = "8 + 1 + 0 + (8 - 0 + 1) + sin(1 + 3)"
F4 = "e^0 + 5^0"
pF = P.parse(F4).printSyntaxTree(1).simplify().printSyntaxTree(1)

#print(regex.match(r"[\+\-\*/\^]|[0-9]+|(?P<brackets>\((?:[^\(\)]|(?0))*\))|([A-Za-z][A-Za-z0-9]*(?P<funcapp>\((?:[^\(\)]|(?0))*\))?)", "").group())

# [sin(4+x), +, -1, *, (x^(3-y)), +, x, *, y, ^, 2, /, u, -, 6, *, tan(x)]

#         _____ + ___________________________
#    [sin(4+x)]                             + ___________________________
#                        [-1, *, (x^(3-y))]             _______________ + _________
#                                                  __ * ________          [-1, *, (6*tan(x))]
#                                                 [x]   [y, ^, 2, /, u, /, y]
