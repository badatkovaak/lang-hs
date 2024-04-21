module Parser() where

import           Control.Applicative
import           Lexer               (Token)
import qualified Text.Parsec         as P

type Parser = P.Parsec [Token] ()





