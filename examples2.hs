
import Data.Maybe
import Lambda.Data
import Lambda.Parse
import Lambda.Evaluate

neval :: Expression -> Int -> Expression
neval e n = iterate eval e !! n

nevals :: String -> Int -> Expression
nevals s n = neval (fromJust (parseWord s)) n

mkD :: String -> Designator
mkD s = Designator s

dToE :: Designator -> Expression
dToE d = ExpressionTerm (TermDesignator d)

lambda :: Designator -> [Expression] -> Expression
lambda d [e] = ExpressionTerm (TermFunction d e)
lambda d es = ExpressionTerm (TermFunction d (ExpressionList es))

a = mkD "a"
b = mkD "b"
c = mkD "c"
d = mkD "d"
x = mkD "x"
y = mkD "y"
z = mkD "z"

lid = lambda x [dToE x]
l1 = lambda x [dToE x]
l2 = lambda x [ExpressionList [dToE x]]
l3 = lambda x [dToE x, dToE y]
l4 = lambda x [dToE x, dToE x]
l5 = lambda x [dToE x, dToE x, dToE x]
l6 = ExpressionList [l1, l1, l1]
l7 = ExpressionList [l4, l4, l4]
l8 = ExpressionList [l5, l5, l5]

lTrue = lambda x [lambda y [dToE x, dToE y]]
lFalse = lambda x [lambda y [dToE y, dToE x]]
lNot = lFalse
lIf = lambda x [lambda y [lambda z [dToE x, dToE y, dToE z]]]
lAnd = lambda x [lambda y [lambda z [lambda a [dToE x, dToE y, dToE x, dToE z, dToE a]]]]
lNand = lambda x [lambda y [lambda z [lambda a [dToE x, dToE y, dToE x, dToE a, dToE z]]]]

lYcomb = lambda y [lambda x [dToE y, dToE x, dToE x], lambda x [dToE y, dToE x, dToE x]]
lInfrep = lambda y [lambda x [dToE y, dToE x, dToE x, dToE x], lambda x [dToE y, dToE x, dToE x, dToE x]]

lIfTrue = ExpressionList [lIf, lTrue]
lIfFalse = ExpressionList [lIf, lFalse]
lIfTrueTrueFalse = ExpressionList [lIf, lTrue, lTrue, lFalse]

