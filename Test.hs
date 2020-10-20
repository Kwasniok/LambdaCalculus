
module Test where

import Text.ParserCombinators.ReadP
import Data.List (intercalate)

parse = readP_to_S

-- types
data Designator = Designator String
data Term = TermDesignator Designator | TermFunction Designator Expression
data Expression = ExpressionTerm Term | ExpressionList [Expression]

-- Show types

instance Show Designator where
    show (Designator s) = s

instance Show Term where
    show (TermDesignator d) = show d
    show (TermFunction d e) = "(\\" ++ (show d) ++ ". " ++ (show e) ++ ")"

instance Show Expression where
    show (ExpressionTerm t) = show t
    show (ExpressionList es) = "(" ++ (intercalate " " (map show es) ) ++")"


-- composition

takeSnd :: ReadP a -> ReadP b -> ReadP b
takeSnd f g = do f; x <- g; return x

interlaced1 :: ReadP a -> ReadP b -> ReadP [a]
interlaced1 f g = do e <- f; es <- many (takeSnd g f); return (e:es)


-- grammar sets

whiteSpaceChar = satisfy (\c -> c == ' ' || c == '\t' || c == '\n')
whiteSpace = many whiteSpaceChar
digit = satisfy (\c -> c >= '0' && c <= '9')
lowerLetter= satisfy (\c -> c >= 'a' && c <= 'z')
upperLetter= satisfy (\c -> c >= 'A' && c <= 'Z')
lexical = choice [digit, lowerLetter, upperLetter]
lambda = char '\\'
lambdaSep = char '.'
openParent = char '('
closeParent = char ')'


-- parser

integer :: ReadP Int
integer = do n <- many1 digit; return (read n)

designator :: ReadP Designator
designator = do s <- lowerLetter; ss <- (many lexical); return (Designator (s:ss))

lambdaHead :: ReadP Designator
lambdaHead = do lambda; whiteSpace; d <- designator; whiteSpace; lambdaSep; return d

termDesignator :: ReadP Term
termDesignator = do d <- designator; return (TermDesignator d)

termFunction :: ReadP Term
termFunction = do d <- lambdaHead; whiteSpace; e <- expression; return (TermFunction d e)

term :: ReadP Term
term = choice [termDesignator, termFunction]

expressionTerm :: ReadP Expression
expressionTerm = do t <- term; return (ExpressionTerm t)

expressionList :: ReadP Expression
expressionList = do openParent; whiteSpace; es <- (interlaced1 expression whiteSpace); whiteSpace; closeParent; return (ExpressionList es)

expression :: ReadP Expression
expression = choice [expressionTerm, expressionList]

word :: ReadP Expression
word = do whiteSpace; e <- expression; whiteSpace; eof; return e

-- parse word

parseWord :: String -> Maybe Expression
parseWord s = let res = (parse word s) in if ((length res) == 1 && (snd (res !! 0)) == "" ) then (Just (fst (res !! 0))) else Nothing

