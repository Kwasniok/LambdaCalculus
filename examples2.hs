
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

-- boolean algebra
lTrue = lambda x [lambda y [dToE x, dToE y]]
lFalse = lambda x [lambda y [dToE y, dToE x]]
lNot = lFalse
lIf = lambda x [lambda y [lambda z [dToE x, dToE y, dToE z]]]
lAnd = lambda x [lambda y [lambda z [lambda a [dToE x, dToE y, dToE x, dToE z, dToE a]]]]
lNand = lambda x [lambda y [lambda z [lambda a [dToE x, dToE y, dToE x, dToE a, dToE z]]]]

-- schoenfield and more
lI = lambda x [dToE x]
lK = lambda x [lambda y [dToE x]]
lZ = lambda x [lambda y [lambda z [dToE x, ExpressionList[dToE y, dToE z]]]]
lS = lambda x [lambda y [lambda z [ExpressionList[dToE x, dToE z], ExpressionList [dToE y, dToE z]]]]
lT = lambda x [lambda y [lambda z [dToE x, dToE z, dToE y]]]
lY = lambda y [lambda x [dToE y, dToE x, dToE x], lambda x [dToE y, dToE x, dToE x]]

lInfexpand = lambda y [lambda x [dToE y, dToE x, dToE x, dToE x], lambda x [dToE y, dToE x, dToE x, dToE x]]

lIfTrue = ExpressionList [lIf, lTrue]
lIfFalse = ExpressionList [lIf, lFalse]
lIfTrueTrueFalse = ExpressionList [lIf, lTrue, lTrue, lFalse]

