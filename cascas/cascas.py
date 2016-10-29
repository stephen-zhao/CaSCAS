import regex
import math

# Options flags consts (temporary)
FLG_NORMAL = FLG_RESET = 0b000
FLG_DEBUG = 0b001
FLG_LANG_RACKET = 0b010
FLG_LANG_HASKELL = 0b100

# OPTION FLAGS
g_optionFlags = FLG_DEBUG


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
        if g_optionFlags & FLG_DEBUG:
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
    KEYWORD = "expt" if g_optionFlags & FLG_LANG_RACKET else "^"

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


# Collection of methods to parse user input
class Parser:
    def __init__(self):
        self.TOKENIZING_REGEX_STRING \
            = r"[\+\-\*/\^]"\
            r"|([0-9]+(\.[0-9]+)?)"\
            r"|(?P<brackets>\((?:[^\(\)]|(?0))*\))"\
            r"|([A-Za-z][A-Za-z0-9]*(?P<funcapp>\((?:[^\(\)]|(?0))*\))?)"

        self.LIST_KNOWN_FUNCTION_NAMES \
            = [
                "sin", "cos", "tan", "sec", "csc", "cot",
                "arcsin", "arccos", "arctan", "arcsec", "arccsc", "arccot",
                "log", "ln"
            ]

    def __repr__(self):
        return (
            "<" + type(self).__name__ + ">"
        )

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
            if regex.match(r"\s", s[ci]):
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
            if regex.match(r"([0-9]+(\.[0-9]+)?)", lt[0]):
                return QNode(lt[0])
            # check for known functions
            for func in self.LIST_KNOWN_FUNCTION_NAMES:
                if (lt[0].startswith(func) and
                        lt[0][len(func)] == '(' and
                        lt[0][-1] == ')'):
                    return FuncNode(
                        func,
                        self.parseTokens(
                            self.tokenize(lt[0][len(func) + 1:-1])
                        )
                    )
            # otherwise is a symbol of some sort
            return SymNode(lt[0])

        # check for addition
        for ti in range(0, len(lt)):
            # if lt[ti] == "-":
            #     newltfromti = self.subtoplus(lt[ti:])
            # # new lt elements for elements from ti to end
            #     lt = lt[0:ti] + newltfromti[:]
            if lt[ti] == "+":
                return AddNode(
                    self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:])
                )
        # check for subtraction
        for ti in range(len(lt) - 1, -1, -1):
            if lt[ti] == "-":
                if ti == 0:
                    return MultNode(
                        QNode(-1),
                        self.parseTokens(lt[ti + 1:])
                    )
                else:
                    return SubNode(
                        self.parseTokens(lt[:ti]),
                        self.parseTokens(lt[ti + 1:])
                    )
        # check for multiplication
        for ti in range(0, len(lt)):
            if lt[ti] == "*":
                return MultNode(
                    self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:])
                )
        # check for division
        for ti in range(len(lt) - 1, -1, -1):
            if lt[ti] == "/":
                return DivNode(
                    self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:])
                )
        # check for exponentiation
        for ti in range(0, len(lt)):
            if lt[ti] == "^":
                return ExpNode(
                    self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:])
                )

    # (Listof Token) -> (Listof Token)
    # deprecated
    def subtoplus(self, lt):
        # takes in a list of tokens in the form of
        #   ["-", t1, t2, ... tn-1, regex"[+-]", ...]
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


def testTokenize(ll_te, b_verbose=False):
    print("==== TestTokenize begin ====")
    P = Parser()
    ll_terp = []
    i = 0
    for test, expected in ll_te:
        if b_verbose:
            print("## TEST", i, "##")
        res = P.tokenize(test)
        if b_verbose:
            print("test string:", test)
        if b_verbose:
            print("res  tokens:", res)
        if b_verbose:
            print("expt tokens:", expected)
        passed = (res == expected)
        if b_verbose:
            print("test was", "SUCCESS" if passed else "FAIL")
        ll_terp.append([test, expected, res, passed])
        i += 1

    print("==== TestTokenize end ====")

    numPassed = sum(map(lambda x: x[3], ll_terp), 0)
    numFailed = i - numPassed
    print("tests passed:", numPassed)
    print("tests failed:", numFailed)

    return ll_terp


def testSimplify(ll_te, b_verbose=False):
    print("==== TestSimplify begin ====")
    P = Parser()
    ll_terp = []
    i = 0
    for test, expected in ll_te:
        if b_verbose:
            print("## TEST", i, "##")
        f = P.parse(test)
        if b_verbose:
            print("test tree:", f)
        res = f.simplify()
        if b_verbose:
            print("res  tree:", res)
        if b_verbose:
            print("expt tree:", expected)
        passed = (str(res) == expected)
        if b_verbose:
            print("test was", "SUCCESS" if passed else "FAIL")
        ll_terp.append([test, expected, res, passed])
        i += 1

    print("==== TestSimplify end ====")

    numPassed = sum(map(lambda x: x[3], ll_terp), 0)
    numFailed = i - numPassed
    print("tests passed:", numPassed)
    print("tests failed:", numFailed)

    return ll_terp


# test code
if __name__ == "__main__":

    P = Parser()
    # test tokenizer
    ll_te = []  # list of list of str and tokens
    # with simple binary ops
    ll_te.append([
        "5+37 +4/ 6- 7*8",
        ["5", "+", "37", "+", "4", "/", "6", "-", "7", "*", "8"]
    ])
    # with brackets
    ll_te.append([
        "5-(5 + 6) /99*(22-8*2 )- (23 *2)",
        ["5", "-", "(5 + 6)", "/", "99", "*",
         "(22-8*2 )", "-", "(23 *2)"]
    ])
    # with random whitespace
    ll_te.append([
        "5  +   9    *10\n-22  /    2",
        ["5", "+", "9", "*", "10", "-", "22", "/", "2"]
    ])
    # with funcapps
    ll_te.append([
        "sin(22 +  99)-22+9*arccos(22)/ln( 4 *2)",
        ["sin(22 +  99)", "-", "22", "+", "9", "*",
         "arccos(22)", "/", "ln( 4 *2)"]
    ])
    # with decimal numbers
    ll_te.append([
        "tan(22.8+6)-6.665/811.28",
        ["tan(22.8+6)", "-", "6.665", "/", "811.28"]
    ])
    # with random symbols
    ll_te.append([
        "log(x)/csc(cooka)*k+88*j",
        ["log(x)", "/", "csc(cooka)", "*", "k",
         "+", "88", "*", "j"]
    ])
    # with negatives (unary op)
    ll_te.append([
        "sin(-2)+(-4)+(-x)*(-k)",
        ["sin(-2)", "+", "(-4)", "+",
         "(-x)", "*", "(-k)"]
    ])
    ll_terp = testTokenize(
        ll_te,
        b_verbose=(g_optionFlags & FLG_DEBUG)
    )

    ll_te = []  # list of list of test and expected
    # FuncNode
    ll_te.append([
        "cos(5 + 3)",
        "(cos (+ 5 3))"
    ])
    # AddNode with 0 addend
    ll_te.append([
        "2 + 3 + 0 + 4",
        "(+ 2 (+ 3 4))"
    ])
    # AddNode with lots of 0 addend
    ll_te.append([
        "0 + 0 + 33 + 0 + 0 + 0",
        "33"
    ])
    # SubNode with 0s
    ll_te.append([
        "3 - 0 + 5 - 8 + 99 * 20 - 0 + 99 + 0 - 11",
        "(+ 3 (+ (- 5 8) (+ (* 99 20) (+ 99 (- 0 11)))))"
    ])
    # MultNode with 0s
    ll_te.append([
        "0 * 9 + 8 * 2 + 7 * 0 * 0 + 9 * 7 * 2 * 0",
        "(* 8 2)"
    ])
    # MultNode with 1s
    ll_te.append([
        "1 * 2 * 3 * 5 * 1 * 1 * 6 * 1 * 1",
        "(* 2 (* 3 (* 5 6)))"
    ])
    # DivNode with 1s
    ll_te.append([
        "x * 5 + 2 / 1 + 8 / 2 / 1 / 5",
        "(+ (* x 5) (+ 2 (/ (/ 8 2) 5)))"
    ])
    # TODO: test case for divide by zero
    # ExpNode with 0 exponent
    ll_te.append([
        "e^0 + 5^0",
        "(+ 1 1)"
    ])
    # ExpNode with 1 exponent
    ll_te.append([
        "e^1 + 5^1",
        "(+ e 5)"
    ])
    # ExpNode with 0 base
    ll_te.append([
        "0^e + 0^7",
        "0"
    ])
    # QNode with fraction 01
    ll_te.append([
        "1.5",
        "15/10"
    ])
    # QNode with fraction 02
    ll_te.append([
        "0.001",
        "1/1000"
    ])
    # Negative unary
    ll_te.append([
        "-24.56+(-x*9)+(-7^y)",
        "(+ (* -1 2456/100) (+ (* -1 (* x 9)) (* -1 (^ 7 y))))"
    ])
    # Negative unary multiplication
    ll_te.append([
        "-7*(-6)",
        "(* -1 (* 7 (* -1 6)))"
    ])
    # randoms 01
    ll_te.append([
        "8 + 1 + 0 + (8 - 0 + 1) + sin(1 + 3)",
        "(+ 8 (+ 1 (+ (+ 8 1) (sin (+ 1 3)))))"
    ])
    # Bunch of stuff altogether
    ll_te.append([
        "sin(4+x)-x^(3-y)+x*y^2/u-6*tan(x)-2*x+y^(x)",
        "(+ (- (sin (+ 4 x)) (^ x (- 3 y))) "
        "(+ (- (- (* x (/ (^ y 2) u)) (* 6 (tan x))) "
        "(* 2 x)) (^ y x)))"
    ])
    # random AddNode
    ll_te.append([
        "1 +0 + 1",
        "(+ 1 1)"
    ])
    # randoms 01
    ll_te.append([
        "8 + 1 + 0 + (8 - 0 + 1) + sin(1 + 3)",
        "(+ 8 (+ 1 (+ (+ 8 1) (sin (+ 1 3)))))"
    ])

    ll_terp = testSimplify(
        ll_te,
        b_verbose=(g_optionFlags & FLG_DEBUG)
    )
    # P = Parser()
    # P.parse("-24.56").print()
    # "-2456/100"


    # print(regex.match(r"[\+\-\*/\^]|[0-9]+|(?P<brackets>\((?:[^\(\)]|(?0))*\))|
    # ([A-Za-z][A-Za-z0-9]*(?P<funcapp>\((?:[^\(\)]|(?0))*\))?)", "").group())

    # [sin(4+x), +, -1, *, (x^(3-y)), +, x, *, y, ^, 2, /, u, -, 6, *, tan(x)]
