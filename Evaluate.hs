
import Data

-- (naive) substitution
-- @note: do not substitue bound designators

subs :: Designator -> Term -> Expression -> Expression
subs d1 t2 (ExpressionTerm t) = ExpressionTerm (subsTerm d1 t2 t)
subs d1 t2 (ExpressionList es) = ExpressionList (map (subs d1 t2) es)

subsTerm :: Designator -> Term -> Term -> Term
subsTerm d1 t2 (TermDesignator d) = if (d == d1) then t2 else (TermDesignator d)
subsTerm d1 t2 (TermFunction d e) = TermFunction d (subs d1 t2 e)


-- replace

replace :: Designator -> Designator -> Expression -> Expression
replace d1 d2 (ExpressionTerm t) = ExpressionTerm (replaceTerm d1 d2 t)
replace d1 d2 (ExpressionList es) = ExpressionList (map (replace d1 d2) es)

replaceTerm :: Designator -> Designator -> Term -> Term
replaceTerm d1 d2 (TermDesignator d) = TermDesignator (replaceDesignator d1 d2 d)
replaceTerm d1 d2 (TermFunction d e) = TermFunction (replaceDesignator d1 d2 d) (replace d1 d2 e)

replaceDesignator :: Designator -> Designator -> Designator -> Designator
replaceDesignator d1 d2 d = if (d == d1) then d2 else d

