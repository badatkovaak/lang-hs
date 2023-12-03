{-# LANGUAGE OverloadedStrings #-}

module Lexer where 

import Data.Text(Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

data Token = Number Double | Plus | Minus | Mult | Div | Power | LParen | RParen

type Parser = MP.Parsec Void Text

sc::Parser ()
sc = L.space C.space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "/*")  

symbol = L.symbol sc
lexeme = L.lexeme sc

lparen = symbol "("
rparen = symbol ")"
plus = symbol "+"
minus = symbol "-"
mult = symbol "*"
divide = symbol "/"
power = symbol "^"

-- minus'::Maybe (Parser Text)
-- minus' = MP.try ( C.string "-" <|> 

-- number::Parser Text
-- number = C.char '-' <*> digits <*> C.string "." <*> digits where 
--     digits =T.pack <$> MP.many C.digitChar
    
-- lexeme number' where
    -- number' = 
    -- where dot = symbol "."
    --         minus' = MP.optional minus
    --         digits = T.pack <$> MP.many C.digitChar

parseToken::Parser Text
parseToken = MP.choice [lparen, rparen, plus, minus, mult, divide, power]

-- parseL

-- parseLParen s = 

-- parseToken s = MP.try (C.char '(' <|> C.char ')' )  
