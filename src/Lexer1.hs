-- {-# LANGUAGE TemplateHaskell #-}

module Lexer1() where

import           Data.Char
import qualified Data.List   as L
import           Data.Maybe  (fromMaybe)
import           Text.Parsec

data Token = Number Double | Plus | Minus | Mult | Div | Power | LParen | RParen
    deriving (Show, Eq)

type Parser = Parsec String ()

processNum::String -> Token
processNum ('-' : xs ) = Number $ negate n where
    Number n = processNum xs
processNum a = Number $ read (xs' ++ ys') where
    b = fromMaybe 0 (L.elemIndex '.' a)
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
     a   -> processNum a

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


type Terminal = Token

-- data ETerminal = Token | Epsilon

data NonTerminal = E | Z | T | Y | F | X | P
    deriving (Show, Eq)

data TerminalNE = T1 Terminal | Epsilon
    deriving (Show, Eq)

data Term = T2 TerminalNE | T3  NonTerminal
    deriving (Show, Eq)

data Rule = Rule NonTerminal [Term]

rules = [
        Rule E [T3 T, T3 Z],
        Rule Z [T2 (T1 Plus), T3 T, T3 Z],
        Rule Z [T2 (T1 Minus), T3 T, T3 Z],
        Rule Z [T2 Epsilon],
        Rule T [T3 F, T3 Y],
        Rule Y [T2 (T1 Mult), T3 F, T3 Y],
        Rule Y [T2 (T1 Div), T3 F, T3 Y],
        Rule Y [T2 Epsilon],
        Rule F [T3 P, T3 X],
        Rule X [T2 (T1 Power), T3 P, T3 X],
        Rule X [T2 Epsilon],
        Rule P [T2 (T1 LParen), T3 E, T2 (T1 RParen)],
        Rule P [T2 Epsilon]
        ]

flatten :: [[a]] -> [a]
flatten a = [ x | b <- a, x <- b]

union::Eq a => [a] -> [a] -> [a]
union (x:xs) ys = if x `L.elem` ys then xs `union` ys else x: xs `union` ys

-- disjunction::Eq a => a -> [a] ->

searchRules::[Rule] -> NonTerminal -> [Rule]
searchRules rs nt = filter (\(Rule x _) -> x == nt) rs

computeFirst::[Rule] -> NonTerminal -> [Term]
computeFirst rs nt = flatten $ map (func nt) $ searchRules rs nt
    where
        func nt (Rule _ (x:xs)) = case x of
            T2 Epsilon -> [T2 Epsilon]
            T2 (T1 t)  -> [T2 (T1 t)]
            T3 nt'     -> checkForEps xs $ computeFirst rs nt'
        checkForEps p xs' = if T2 Epsilon `elem` xs'
            then union xs' $ filter (/= T2 Epsilon) p
            else xs'

-- computeFisrts::[Rule] -> [[NonTerminal]]
-- computeFisrts rs =

-- computeFollow::[Rule] -> [[NonTerminal]]
-- computeFollow
