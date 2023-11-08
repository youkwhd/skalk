module Lexer where

import Data.String
import Data.Char (isDigit)

data Token = Token {
    literal :: String,
    _type :: TokenType
}

data TokenType =
    UNKNOWN    |
    WHITESPACE |
    FUNC       |
    LPAREN     |
    RPAREN     |
    NUMBER     |
    NEWLINE    |
    PLUS       |
    MINUS      |
    TIMES      |
    DIVIDE deriving (Eq, Ord, Enum, Show)

consumeWith :: String -> (String -> Bool) -> (String, String)
consumeWith "" _ = ("", "")
consumeWith str fn = 
                    if fn str then 
                        let next = consumeWith (tail str) fn in
                        (head str : fst next, snd next)
                    else
                        ("", str)
        
readNumber :: String -> (Token, String)
readNumber str = 
            let consumed = consumeWith str (\(ch:_) -> isDigit ch || ch == '.') in
            let result = fst consumed in
            (Token {literal = result, _type = if result == "" then UNKNOWN else NUMBER}, snd consumed)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (ch:str) = case ch of
    ' ' -> Token {literal = " ", _type = WHITESPACE} : tokenize str
    '\t' -> Token {literal = "\t", _type = WHITESPACE} : tokenize str
    '(' -> Token {literal = "(", _type = LPAREN} : tokenize str
    ')' -> Token {literal = ")", _type = RPAREN} : tokenize str
    '+' -> Token {literal = "+", _type = PLUS} : tokenize str
    '-' -> Token {literal = "-", _type = MINUS} : tokenize str
    '*' -> Token {literal = "*", _type = TIMES} : tokenize str
    '/' -> Token {literal = "/", _type = DIVIDE} : tokenize str
    ch | isDigit ch ->
        let consumed = readNumber (ch : str) in
        fst consumed : tokenize (snd consumed)
    ch -> Token {literal = ch : "", _type = UNKNOWN} : tokenize str
