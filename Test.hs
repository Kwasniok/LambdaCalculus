
module Test where

import Text.ParserCombinators.ReadP
import Data.List (intercalate)

parse = readP_to_S

-- types
-- Int
data Designator = Designator String
data Expression = ExpressionDesignator Designator | ExpressionFunction Designator Expression | ExpressionList [Expression]


-- Show types

instance Show Designator where
    show (Designator s) = s

instance Show Expression where
    show (ExpressionDesignator d) = show d
    show (ExpressionFunction d e) = "(\\" ++ (show d) ++ ". " ++ (show e) ++ ")"
    show (ExpressionList es) = "(" ++ (intercalate " " (map show es) ) ++")"


-- composition

takeSnd :: forall a. forall b. ReadP a -> ReadP b -> ReadP b
takeSnd f g = do f; x <- g; return x

interlaced1 :: forall a. forall b. ReadP a -> ReadP b -> ReadP [a]
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

expressionDesignator :: ReadP Expression
expressionDesignator = do d <- designator; return (ExpressionDesignator d)

expressionFunction :: ReadP Expression
expressionFunction = do d <- lambdaHead; whiteSpace; e <- expression; return (ExpressionFunction d e)

expressionList :: ReadP Expression
expressionList = do openParent; whiteSpace; es <- (interlaced1 expression whiteSpace); whiteSpace; closeParent; return (ExpressionList es)

expression :: ReadP Expression
expression = choice [expressionDesignator, expressionFunction, expressionList]


word :: ReadP Expression
word = do whiteSpace; e <- expression; whiteSpace; eof; return e

-- parse word

parseWord :: String -> Maybe Expression
parseWord s = let res = (parse word s) in if ((length res) == 1 && (snd (res !! 0)) == "" ) then (Just (fst (res !! 0))) else Nothing

