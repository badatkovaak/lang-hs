module Lexer1 where 

import Text.Parsec
-- import Text.ParserCombinators.Parsec
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)

data Token = Number Double | Plus | Minus | Mult | Div | Power | LParen | RParen

type Parser = Parsec String () 

processNum::String -> Token
processNum ('-' : xs ) = Number (-n) where 
    Number n = processNum xs
processNum a = Number $ read (xs ++ ys) where 
    b = fromMaybe 0 (elemIndex '.' a) 
    (xs,ys) = splitAt b a
    xs' = if null $ init xs then  "0" else init xs
    ys' = if null ys then "0" else ys
    
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

parseToken::Parser String
parseToken = try(string "+") <|> try( string "*") <|> try(string "/") <|> try(string "^") <|> try( string "(") <|> try(string ")") <|> try number <|> try (string "-")
  where
    number = (++) <$> option "" (string "-") <*> ((++) <$> many digit <*> ( (++) <$> string "." <*> many digit))

