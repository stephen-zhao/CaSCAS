from shared import Shared
from syntax_parser import Parser


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


def test():
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
    testTokenize(
        ll_te,
        b_verbose=(Shared.g_optionFlags & Shared.FLG_DEBUG)
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
    testSimplify(
        ll_te,
        b_verbose=(Shared.g_optionFlags & Shared.FLG_DEBUG)
    )


if __name__ == "__main__":
    test()
