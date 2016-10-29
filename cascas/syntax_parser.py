from syntax import *
import regex


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
