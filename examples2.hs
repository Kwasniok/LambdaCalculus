
import Data.Maybe
import Data
import Parse
import Evaluate

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

fid = lambda x [dToE x]
f1 = lambda x [dToE x]
f2 = lambda x [ExpressionList [dToE x]]
f3 = lambda x [dToE x, dToE y]
f4 = lambda x [dToE x, dToE x]
f5 = lambda x [dToE x, dToE x, dToE x]

e1 = ExpressionList [f1, f1, f1]
e2 = ExpressionList [f4, f4, f4]
e3 = ExpressionList [f5, f5, f5]

fTrue = lambda x [lambda y [dToE x, dToE y]]
fFalse = lambda x [lambda y [dToE y, dToE x]]
fNot = fFalse
fIf = lambda x [lambda y [lambda z [dToE x, dToE y, dToE z]]]

eIfTrue = ExpressionList [fIf, fTrue]
eIfFalse = ExpressionList [fIf, fFalse]
eIfTrueTrueFalse = ExpressionList [fIf, fTrue, fTrue, fFalse]

