
import Data.Maybe
import Lambda.Data
import Lambda.Parse
import Lambda.Evaluate

neval :: Expression -> Int -> Expression
neval e n = iterate eval e !! n

nevals :: String -> Int -> Expression
nevals s n = neval (fromJust (parseWord s)) n

mkD :: String -> Expression
mkD s = ExpressionTerm (TermDesignator (Designator s))

-- note: first argument must be a designator
lambda :: Expression -> [Expression] -> Expression
lambda (ExpressionTerm (TermDesignator d)) [e] = ExpressionTerm (TermFunction d e)
lambda (ExpressionTerm (TermDesignator d)) es = ExpressionTerm (TermFunction d (ExpressionList es))

a = mkD "a"
b = mkD "b"
c = mkD "c"
d = mkD "d"
x = mkD "x"
y = mkD "y"
z = mkD "z"

lid = lambda x [x]

-- boolean algebra
lTrue = lambda x [lambda y [x, y]]
lFalse = lambda x [lambda y [y, x]]
lNot = lFalse
lIf = lambda x [lambda y [lambda z [x, y, z]]]
lAnd = lambda x [lambda y [lambda z [lambda a [x, y, x, z, a]]]]
lNand = lambda x [lambda y [lambda z [lambda a [x, y, x, a, z]]]]

-- schoenfield and more
lI = lambda x [x]
lK = lambda x [lambda y [x]]
lZ = lambda x [lambda y [lambda z [x, ExpressionList[y, z]]]]
lS = lambda x [lambda y [lambda z [ExpressionList[x, z], ExpressionList [y, z]]]]
lT = lambda x [lambda y [lambda z [x, z, y]]]
lY = lambda y [lambda x [y, x, x], lambda x [y, x, x]]

lInfexpand = lambda y [lambda x [y, x, x, x], lambda x [y, x, x, x]]

lIfTrue = ExpressionList [lIf, lTrue]
lIfFalse = ExpressionList [lIf, lFalse]
lIfTrueTrueFalse = ExpressionList [lIf, lTrue, lTrue, lFalse]

