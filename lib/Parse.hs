
module Parse (
    designator,
    term,
    expression,
    word,
    parse,
    parseWord,
) where

import Text.ParserCombinators.ReadP
import Data


-- composition

takeSnd :: ReadP a -> ReadP b -> ReadP b
takeSnd f g = do f; x <- g; return x

interlaced1 :: ReadP a -> ReadP b -> ReadP [a]
interlaced1 f g = do e <- f; es <- many (takeSnd g f); return (e:es)


-- strings

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


-- data

designator :: ReadP Designator
designator = do s <- lowerLetter; ss <- (many lexical); return (Designator (s:ss))

lambdaDesignator :: ReadP Designator
lambdaDesignator = do lambda; whiteSpace; d <- designator; whiteSpace; lambdaSep; return d

termDesignator :: ReadP Term
termDesignator = do d <- designator; return (TermDesignator d)

termFunction :: ReadP Term
termFunction = do d <- lambdaDesignator; whiteSpace; e <- expression; return (TermFunction d e)

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

-- parse

parse :: ReadP a -> String -> Maybe a
parse p s = let res = (readP_to_S p s) in if ((length res) == 1 && (snd (res !! 0)) == "" ) then (Just (fst (res !! 0))) else Nothing

parseWord :: String -> Maybe Expression
parseWord s = parse word s


-- debug parse

parseAllPossible :: ReadP a -> String -> [a]
parseAllPossible p s = map (fst) (readP_to_S p s)
