
module Data (
    Designator (Designator),
    Term (TermDesignator, TermFunction),
    Expression (ExpressionTerm, ExpressionList),
) where

import Data.List (intercalate)

data Designator = Designator String
data Term = TermDesignator Designator | TermFunction Designator Expression
data Expression = ExpressionTerm Term | ExpressionList [Expression]


-- instance Eq

instance Eq Designator where
    (==) (Designator x) (Designator y) = x == y

-- instance Ord

instance Ord Designator where
    compare (Designator x) (Designator y) = compare x y

--instance Show

instance Show Designator where
    show (Designator s) = s

instance Show Term where
    show (TermDesignator d) = show d
    show (TermFunction d e) = "(\\" ++ (show d) ++ ". " ++ (show e) ++ ")"

instance Show Expression where
    show (ExpressionTerm t) = show t
    show (ExpressionList es) = "(" ++ (intercalate " " (map show es) ) ++")"

