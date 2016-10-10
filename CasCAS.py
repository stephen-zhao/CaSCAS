import re
import regex


# class Token:
#     # | "(...)"
#     # | "az(...)"
#     # | "..."
#     def __init__(self, s):
#         self.__clist = list(s)

#     def __repr__(self):
#         return "".join(self.__clist)

#     def __len__(self):
#         return len(self.__clist)

#     def __getitem__(self, key):
#         return self.__clist[key]

#     def __iter__(self):
#         self.i = 0
#         return self

#     def __eq__(self, other):
#         if type(other) is str:
#             return self.__str__() == other
#         elif type(other) is Token:
#             return self.__str__() == Token.__str__()
#         else:
#             return False

#     def next(self):
#         if self.i < len(self):
#             i = self.i
#             self.i += 1
#             return self[i]
#         else:
#             raise StopIteration()

#     def addChar(self, c):
#         self.__clist.append(c)

#     def startsWith(self, s, start=0, end=None):
#         return self.__repr__().startswith(s, start, end)


class SyntaxNode:
    # | AddNode
    # | SubNode
    # | MultNode
    # | DivNode
    # | ExpNode
    # | ConstRational
    # | Symbol

    def __init__(self):
        self.keyword = ""   # keyword to identify it
        self.lsn = []       # list of sub-nodes

    @classmethod
    def fromCustom(cls, keyword, *nodes):
        inst = cls()
        inst.keyword = keyword
        inst.lsn = list(nodes)
        return inst

    def __repr__(self):
        if not self.lsn:
            return self.keyword
        else:
            return "(" + self.keyword + " " + " ".join(map(lambda n: n.__repr__(), self.lsn)) + ")"

    def printSyntaxTree(self):
        print(self.__repr__())


class AddNode(SyntaxNode):
    def __init__(self, *addends):
        self.keyword = "+"
        self.lsn = list(addends)

    def addends(self):
        return self.lsn

class SubNode(SyntaxNode):
    def __init__(self, minuend, subtractend):
        self.keyword = "-"
        self.lsn = [minuend, subtractend]

    def minuend(self):
        return self.lsn[0]

    def subtractend(self):
        return self.lsn[1]


class MultNode(SyntaxNode):
    def __init__(self, *multiplicands):
        self.keyword = "*"
        self.lsn = list(multiplicands)

    def multiplicands(self):
        return self.lsn


class DivNode(SyntaxNode):
    def __init__(self, dividend, divisor):
        self.keyword = "/"
        self.lsn = [dividend, divisor]

    def dividend(self):
        return self.lsn[0]

    def divisor(self):
        return self.lsn[1]


class ExpNode(SyntaxNode):
    def __init__(self, base, exponent):
        self.keyword = "^"
        self.lsn = [base, exponent]

    def base(self):
        return self.lsn[0]

    def exponent(self):
        return self.lsn[1]


class ConstRational(SyntaxNode):
    def __init__(self, primitiveValue):
        self.keyword = "constRational"
        if type(primitiveValue) is int:
            self.lsn = [primitiveValue, 1]
        elif type(primitiveValue) is float:
            self.lsn = list(primitiveValue.as_integer_ratio())
        else:
            self.lsn = [int(primitiveValue), 1]

    def approxVal(self):
        return (1.0 * self.lsn[0]) / (1.0 * self.lsn[1])

    def numerator(self):
        return self.lsn[0]

    def denominator(self):
        return self.lsn[1]


class Symbol(SyntaxNode):
    def __init__(self, symbol):
        self.keyword = symbol
        self.lsn = []


class Parser:

    def __init__(self):
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

    # # Str -> (Listof Token)
    # def tokenize_old(self, s):
    #     # lt: list of tokens
    #     # ct: current token
    #     # ci: current char
    #     # loop through s,
    #     ci = 0
    #     lt = []
    #     while ci < len(s):
    #         # regex match to tokenize:
    #         # if regex"\(":
    #         #   count parentheses (increment ci in a loop)
    #         #       when parentheses count > 0, we just add s[ci] to ct.string
    #         #       when parentheses count == 0, append ct to lt, break
    #         if re.match("\(", s[ci]):
    #             ct = s[ci]
    #             parenCount = 1
    #             ci += 1
    #             while parenCount > 0:
    #                 if ci >= len(s):
    #                     self.printTokenizeErr(s, ci, ct)
    #                     return []
    #                 if re.match("\(", s[ci]):
    #                     parenCount += 1
    #                 elif re.match("\)", s[ci]):
    #                     parenCount -= 1
    #                 ct.addChar(s[ci])
    #                 ci += 1
    #             lt.append(ct)

    #         # elif regex"[\+\-\*/\^]":
    #         #   ct = new Token(s[ci])
    #         #   append ct to lt
    #         elif re.match("[\+\-\*/\^]", s[ci]):
    #             ct = s[ci]
    #             ci += 1
    #             lt.append(ct)

    #         # elif regex"[0-9]":
    #         #   ct = new Token(s[ci])
    #         #   check for digits (in loop)
    #         #       if digit, ct.string += s[ci]; increment ci
    #         #       otherwise append ct to lt, break
    #         elif re.match("[0-9]", s[ci]):
    #             ct = s[ci]
    #             ci += 1
    #             while ci < len(s) and re.match("[0-9]", s[ci]):
    #                 ct.addChar(s[ci])
    #                 ci += 1
    #             lt.append(ct)

    #         # elif regex"[A-Za-z]":
    #         #   ct = new Token(s[ci])
    #         #   check for: (in loop)
    #         #       if regex"[A-Za-z0-9]", ct.string += s[ci]; increment ci
    #         #       if regex"\(", count parentheses
    #         #       otherwise append ct to lt, break
    #         elif re.match("[A-Za-z]", s[ci]):
    #             ct = s[ci]
    #             ci += 1
    #             parenCount = -1
    #             while ci < len(s) and re.match("[A-Za-z0-9]", s[ci]):
    #                 ct.addChar(s[ci])
    #                 ci += 1
    #                 if (parenCount == -1 and
    #                         ci < len(s) and
    #                         re.match("\(", s[ci])):
    #                     ct.addChar(s[ci])
    #                     parenCount = 1
    #                     ci += 1
    #                     while parenCount > 0:
    #                         if ci >= len(s):
    #                             self.printTokenizeErr(s, ci, ct)
    #                             return []
    #                         if re.match("\(", s[ci]):
    #                             parenCount += 1
    #                         elif re.match("\)", s[ci]):
    #                             parenCount -= 1
    #                         ct.addChar(s[ci])
    #                         ci += 1
    #                     break
    #             lt.append(ct)

    #         # elif " ":
    #         #   ignore and move on
    #         elif s[ci] == ' ':
    #             ci += 1

    #         # else:
    #         #   cannotTokenize(ct,c)
    #         else:
    #             self.printTokenizeErr(s, ci)
    #             return []

    #     return lt

    #     # lt: list of tokens
    #     # ct: current token
    #     # ci: current char
    #     # loop through s,
    #     # regex match to tokenize:
    #     # if regex"\(":
    #     #   count parentheses (increment ci in a loop)
    #     #       when parentheses count > 0, we just add s[ci] to ct.string
    #     #       when parentheses count == 0, append ct to lt, break
    #     # elif regex"[\+\-\*/\^]":
    #     #   ct = new Token(s[ci])
    #     #   append ct to lt
    #     # elif regex"[0-9]":
    #     #   ct = new Token(s[ci])
    #     #   check for digits (in loop)
    #     #       if digit, ct.string += s[ci]; increment ci
    #     #       otherwise append ct to lt, break
    #     # elif regex"[A-Za-z]":
    #     #   ct = new Token(s[ci])
    #     #   check for: (in loop)
    #     #       if regex"[A-Za-z0-9]", ct.string += s[ci]; increment ci
    #     #       if regex"\(", count parentheses
    #     #       otherwise append ct to lt, break
    #     # elif " ":
    #     #   ignore and move on
    #     # else:
    #     #   cannotTokenize(ct,c)
    #     #
    #     # return lt

    # Str -> SyntaxNode
    def parse(self, s):
        return self.parseTokens(self.tokenize(s))

    # (listof Token) -> SyntaxNode
    def parseTokens(self, lt):
        # check for base cases
        if len(lt) == 1:
            # check for parenthesized statements (...)
            if lt[0][0] == '(' and lt[0][-1] == ')':
                return self.parseTokens(self.tokenize(lt[0][1:-1]))
            # check for numerical numbers
            if regex.match(r"\-?[0-9]+", lt[0].__repr__()):
                return ConstRational(lt[0])
            # check for known functions
            for func in self.LIST_KNOWN_FUNCTION_NAMES:
                if lt[0].startswith(func) and \
                        lt[0][len(func)] == '(' and \
                        lt[0][-1] == ')':
                    return SyntaxNode.fromCustom(
                        func,
                        self.parseTokens(self.tokenize(lt[0][len(func)+1:-1]))
                    )
            # otherwise is a symbol of some sort
            return Symbol(lt[0])

        # check for - and +
        for ti in range(0, len(lt)):
            if lt[ti] == "-":
                newltfromti = self.subtoplus(lt[ti:])    # new lt elements for elements from ti to end
                lt = lt[0:ti] + newltfromti[:]
            if lt[ti] == "+":
                return AddNode(self.parseTokens(lt[:ti]), self.parseTokens(lt[ti + 1:]))
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
F = "sin(4+x)-x^(3-y)+x*y^2/u-6*tan(x)"
print(P.tokenize(F))
print(P.subtoplus(["-", "4", "*", "x", "/", "y", "^", "(2 + e)"]))
pF = P.parse(F).printSyntaxTree()

#print(regex.match(r"[\+\-\*/\^]|[0-9]+|(?P<brackets>\((?:[^\(\)]|(?0))*\))|([A-Za-z][A-Za-z0-9]*(?P<funcapp>\((?:[^\(\)]|(?0))*\))?)", "").group())

# [sin(4+x), +, -1, *, (x^(3-y)), +, x, *, y, ^, 2, /, u, -, 6, *, tan(x)]

#         _____ + ___________________________
#    [sin(4+x)]                             + ___________________________
#                        [-1, *, (x^(3-y))]             _______________ + _________
#                                                  __ * ________          [-1, *, (6*tan(x))]
#                                                 [x]   [y, ^, 2, /, u, /, y]
#
#
#
