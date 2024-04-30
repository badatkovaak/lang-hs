module Parser() where

import           Control.Applicative
import           Lexer                      (Token)
import qualified Text.Megaparsec.Byte.Lexer as P
import qualified Text.Parsec                as P

type Parser = P.Parsec [Token] ()

data Literal = IntLiteral Int | FloatLiteral Double
    deriving (Show)

data BinaryOp = Plus | Minus | Mult | Div
    deriving (Show)

data Var = Id String | Liter Literal
    deriving (Show)

data Expr = LiterExpr Var | BinaryExpr BinaryOp Var Var
    deriving (Show)

-- parseLiteral = P.lexeme
