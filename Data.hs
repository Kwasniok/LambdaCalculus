
module Data (
    Designator (Designator),
    Term (TermDesignator, TermFunction),
    Expression (ExpressionTerm, ExpressionList),
) where

import Data.List (intercalate)

data Designator = Designator String
data Term = TermDesignator Designator | TermFunction Designator Expression
data Expression = ExpressionTerm Term | ExpressionList [Expression]

--instance Show

instance Show Designator where
    show (Designator s) = s

instance Show Term where
    show (TermDesignator d) = show d
    show (TermFunction d e) = "(\\" ++ (show d) ++ ". " ++ (show e) ++ ")"

instance Show Expression where
    show (ExpressionTerm t) = show t
    show (ExpressionList es) = "(" ++ (intercalate " " (map show es) ) ++")"

