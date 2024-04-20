module Lexer where

import           Control.Applicative
import           Data.Char
import qualified Data.List           as L
import           Data.Maybe
import qualified Text.Parsec         as P


data Token =
    LParen | RParen | Semicolon | Comma | Plus | Minus | Mult | Div | Power | IntLiteral Int | FloatLiteral Double | Id String
    deriving (Show, Eq)

type Parser = P.Parsec String ()

space' :: Parser ()
space' = P.skipMany $ P.char ' '

matchInt :: Parser String
matchInt = (++) <$> P.option "" (P.string "-") <*> digits'
    where digits' = P.string "0" <|> (((:) <$> P.oneOf "123456789") <*> P.many P.digit)

parseInt :: Parser Token
parseInt = IntLiteral . read <$> matchInt

parseFloat :: Parser Token
parseFloat = FloatLiteral . read <$> ((++) <$> P.option "" matchInt <*> ((++) <$> ((: []) <$> P.char '.') <*> P.many P.digit))

parseId :: Parser Token
parseId = Id <$> P.many1 P.letter
