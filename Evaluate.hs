
module Evaluate (
    subs,
    replace,
    eval,
) where

import qualified Data.Set as Set
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


-- extract bound designators

boundDesignators :: Expression -> Set.Set Designator
boundDesignators (ExpressionTerm t) = boundDesignatorsTerm t
boundDesignators (ExpressionList es) = foldl Set.union Set.empty (map boundDesignators es)

boundDesignatorsTerm :: Term -> Set.Set Designator
boundDesignatorsTerm (TermDesignator d) = Set.empty
boundDesignatorsTerm (TermFunction d e) = Set.insert d (boundDesignators e)


-- replacement designators (infinite supply of unique designaotrs)

replacementDesignators :: [Designator]
replacementDesignators = [Designator [c] | c <- ['a'..'z']] ++ [Designator ("x" ++ (show n)) | n <- [1..]]


-- replace all bound occurences of designators within a given set with new ones
-- without changing the meaning of an expression

resolveBoundDesignatorOverlap :: Set.Set Designator -> Expression -> Expression
resolveBoundDesignatorOverlap dsToAvoid e = (foldl (.) id repls) e
    where
        dsOld :: [Designator]
        dsOld = Set.toList (Set.filter (\d -> Set.member d dsToAvoid) (boundDesignators e))
        dsNew :: [Designator]
        dsNew = filter (\d -> (not (Set.member d dsToAvoid) || (Set.member d (boundDesignators e)))) replacementDesignators
        repl :: (Designator, Designator) -> (Expression -> Expression)
        repl (dOld, dNew) = replace dOld dNew
        repls :: [Expression -> Expression]
        repls = map repl (zip dsOld dsNew)


-- application

apply :: Designator -> Expression -> Term -> Expression
apply d1 e1 t2 = subs d1 t2 re1
    where re1 = resolveBoundDesignatorOverlap (Set.insert d1 (boundDesignatorsTerm t2)) e1


-- evaluation

eval :: Expression -> Expression
-- single term
eval (ExpressionTerm t) = ExpressionTerm (evalTerm t)
-- function application
eval (ExpressionList ((ExpressionTerm (TermFunction d e) : (ExpressionTerm t) : es))) =
    let f = apply d e t in
        if (length es == 0) then f else ExpressionList (f : es)
-- remove nesting if single expression
eval (ExpressionList [e]) = e
-- other
eval e = e

evalTerm :: Term -> Term
evalTerm (TermDesignator d) = TermDesignator d
evalTerm (TermFunction d e) = TermFunction d (eval e)
