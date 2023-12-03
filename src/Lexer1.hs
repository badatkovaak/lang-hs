module Lexer1 where 

import Text.Parsec
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)

data Token = Number Double | Plus | Minus | Mult | Div | Power | LParen | RParen
    deriving Show

type Parser = Parsec String () 

processNum::String -> Token
processNum ('-' : xs ) = Number $ negate n where 
    Number n = processNum xs
processNum a = Number $ read (xs' ++ ys') where 
    b = fromMaybe 0 (elemIndex '.' a) 
    (xs,ys) = splitAt b a
    xs' = if null xs then  "0" else xs
    ys' = if length ys == 1 then ".0" else ys
    
mapToToken :: String -> Token
mapToToken s = case s of
     "(" -> LParen
     ")" -> RParen
     "+" -> Plus
     "-" -> Minus
     "*" -> Mult
     "/" -> Div
     "^" -> Power
     a -> processNum a

parseNumber::Parser String
parseNumber = (++) <$> option "" (string "-") <*> ((++) <$> many digit <*> ( (++) <$> string "." <*> many digit))

space'::Parser ()
space' = skipMany $ char ' '

parseToken::Parser String
parseToken = space' *> choice ps <* space' where
    ps = [
        try $ string "+" ,
        try $ string "*" ,
        try $ string "/" ,
        try $ string "^" ,
        try $ string "(" ,
        try $ string ")" ,
        try parseNumber ,
        try $ string "-"
        ]

parse'::String -> Either ParseError [String]
parse' = runParser (many parseToken) () ""

tokenize::String -> Either ParseError [Token]
tokenize input = map mapToToken <$> parse' input

-- E -> E + T
-- E -> E - T
-- E -> T
-- T -> T * F
-- T -> T / F
-- T -> F
-- F -> F ^ P
-- F -> P
-- P -> (E)
-- P -> num

