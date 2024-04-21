module Lexer(Token) where

import           Control.Applicative
import           Data.Char
import qualified Data.List           as L
import           Data.Maybe
import qualified Text.Parsec         as P


data Token =
    LParen
    | RParen
    | Semicolon
    | Comma
    | ThinArrow
    | FatArrow
    | Lambda
    | Plus
    | Minus
    | Mult
    | Div
    | Power
    | IntLiteral Int
    | FloatLiteral Double
    | Id String
    deriving (Show, Eq)

type Lexer = P.Parsec String ()

space' :: Lexer ()
space' = P.skipMany $ P.char ' '

-- matchInt :: Lexer String
-- matchInt =

lexInt :: Lexer Token
lexInt = IntLiteral . read <$> ((++) <$> minus <*> digits')
    where minus =  P.option "" (P.string "-")
          digits' = P.string "0" <|> (((:) <$> P.oneOf "123456789") <*> P.many P.digit)

addLastZero :: String -> String
addLastZero s = case last s of
    '.' -> s ++ "0"
    _   -> s

addFirstZero :: String -> String
addFirstZero s = case head s of
    '.' -> '0' : s
    '-' -> case head $ tail s of
        '.' -> "-" ++ "0" ++ tail s
        _   -> s
    _ -> s

lexFloat :: Lexer Token
lexFloat = sToFloat <$> ((++) <$> minus <*> ((++) <$> P.option "" digits' <*> ((++) <$> dot <*> P.many P.digit)))
    where sToFloat = FloatLiteral . read . addFirstZero . addLastZero
          minus = P.option "" (P.string "-")
          digits' = P.string "0" <|> (((:) <$> P.oneOf "123456789") <*> P.many P.digit)
          dot = (: []) <$> P.char '.'

lexId :: Lexer Token
lexId = Id <$> P.many1 P.letter

charToToken :: Char -> Token
charToToken c = case c of
    '+'  -> Plus
    '-'  -> Minus
    '*'  -> Mult
    '/'  -> Div
    '^'  -> Power
    '('  -> LParen
    ')'  -> RParen
    ';'  -> Semicolon
    ','  -> Comma
    '\\' -> Lambda

strToToken s = case s of
    "->" -> ThinArrow
    "=>" -> FatArrow

lexSingle :: Lexer Token
lexSingle = charToToken <$> (P.char '+' <|> P.char '-' <|> P.char '*' <|> P.char '/' <|> P.char '^' <|> P.char '(' <|> P.char ')' <|> P.char ';' <|> P.char ',' <|> P.char '\\')

lexFixed :: Lexer Token
lexFixed = strToToken <$> (P.string "->" <|> P.string "=>")

lexToken :: Lexer Token
lexToken = space' *> (lexSingle <|>P.try lexFixed <|> P.try lexInt <|> P.try lexFloat <|> P.try lexId) <* space'

lexTokens :: Lexer [Token]
lexTokens = P.many1 lexToken
