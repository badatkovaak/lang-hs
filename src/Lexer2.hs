module Lexer2 where

import           Data.Char
import           Data.List
import           Data.Maybe  (fromMaybe)
import           Text.Parsec


type Parser = Parsec String ()

-- data Token = Number Double | Plus | Minus | Mult | Div | Power | LParen | RParen
--     deriving Show

type Number = Double

data TOperator = Plus | Minus | Mult | Div | Power
    deriving Show

data Expr = Node Expr TOperator Expr | TTerminal Number
    deriving Show

space'::Parser ()
space' = skipMany $ char ' '

processNum::String -> Number
processNum ('-' : xs ) = negate $ processNum xs
processNum a = read (xs' ++ ys') where
    b = fromMaybe 0 (elemIndex '.' a)
    (xs,ys) = splitAt b a
    xs' = if null xs then  "0" else xs
    ys' = if length ys == 1 then ".0" else ys

parseNumber::Parser Number
parseNumber = processNum <$> (space' *> num <* space')
    where
        num = (++) <$> option "" (string "-") <*> ((++) <$> many digit <*> ( (++) <$> string "." <*> many digit))

parseOperator::Parser TOperator
parseOperator = match' <$> ( space' *> oneOf "+-*/^" <* space')
    where
        match' a = case a of
            '+' -> Plus
            '-' -> Minus
            '*' -> Mult
            '/' -> Div
            '^' -> Power

parseExpr::Parser Expr
parseExpr = try (between (space' *> char '(') (char ')' <* space') parseNode) <|> (TTerminal <$> parseNumber)

parseNode::Parser Expr
parseNode = Node <$> parseExpr <*> parseOperator <*> parseExpr

parse'::String -> Either ParseError Expr
parse' = runParser parseExpr () ""
