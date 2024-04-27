module Recursion() where

import           Control.Applicative
import           Data.Char
import qualified Data.List           as L
import           Data.Maybe
import qualified Text.Parsec         as P


data BaseFunc =
    S
    | O
    | R
    | I Int Int
    deriving (Show, Eq)

data Token =
    LParen
    | RParen
    | Semicolon
    | ThinArrow
    | FatArrow
    | Comma
    | Dot
    | Lambda
    | Assign
    | IntLiteral Int
    | Id String
    | Func BaseFunc
    deriving (Show, Eq)

type Lexer = P.Parsec String ()

space' :: Lexer ()
space' = P.skipMany $ P.char ' '

intDigits :: Lexer String
intDigits = P.string "0" <|> (((:) <$> P.oneOf "123456789") <*> P.many P.digit)

lexInt :: Lexer Token
lexInt = IntLiteral . read <$> intDigits
    -- where digits' = P.string "0" <|> (((:) <$> P.oneOf "123456789") <*> P.many P.digit)

lexId :: Lexer Token
lexId = Id <$> ((:) <$> P.letter <*> P.many (P.letter <|> P.digit <|> P.oneOf "'_"))

charToToken :: Char -> Token
charToToken c = case c of
    '('  -> LParen
    ')'  -> RParen
    ';'  -> Semicolon
    '\\' -> Lambda
    ','  -> Comma
    '.'  -> Dot
    '='  -> Assign
    'S'  -> Func S
    'O'  -> Func O
    'R'  -> Func R

strToToken s = case s of
    "->" -> ThinArrow
    "=>" -> FatArrow

-- lexInt' = read <$> digits'

convert :: (String, String) -> Token
convert (x,y) = Func (I (read x) (read y))

pair' x y = (x,y)

lexFuncId :: Lexer Token
lexFuncId = convert <$> part1
    where part1 = P.string "I_" *> (pair' <$> intDigits <*> part2)
          part2 = (: []) <$> P.char '^' *> intDigits

lexSingle :: Lexer Token
lexSingle = charToToken <$> (P.char '(' <|> P.char ')' <|> P.char ';' <|> P.char ',' <|> P.char '\\' <|> P.char '=' <|> P.char '.' <|> P.char 'S' <|> P.char 'O' <|> P.char 'R')

lexFixed :: Lexer Token
lexFixed = strToToken <$> (P.string "->" <|> P.string "=>")

lexToken :: Lexer Token
lexToken = space' *> (lexSingle <|>P.try lexFixed <|> P.try lexInt <|> P.try lexFuncId <|> P.try lexId) <* space'

lexTokens :: Lexer [Token]
lexTokens = P.many1 lexToken

type Parser = P.Parsec [Token] ()

-- type Expr = 

-- parseLiteral :: Parser 
-- parseLiteral = 







