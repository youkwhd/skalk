module Lexer where

import Data.String
import Data.Char (isDigit)

data Token = Token {
    literal :: String,
    _type :: TokenType
}

data TokenType
    = UNKNOWN
    | EOF
    | WHITESPACE
    | FUNC
    | LPAREN
    | RPAREN
    | NUMBER
    | NEWLINE
    | PLUS
    | MINUS
    | TIMES
    | DIVIDE
    deriving (Eq, Ord, Enum, Show)

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

nextToken :: String -> (Token, String)
nextToken "" = (Token {literal = "", _type = UNKNOWN}, "")
nextToken (ch:str) = case ch of
    ' ' -> (Token {literal = " ", _type = WHITESPACE}, str)
    '\t' -> (Token {literal = "\t", _type = WHITESPACE}, str)
    '(' -> (Token {literal = "(", _type = LPAREN}, str)
    ')' -> (Token {literal = ")", _type = RPAREN}, str)
    '+' -> (Token {literal = "+", _type = PLUS}, str)
    '-' -> (Token {literal = "-", _type = MINUS}, str)
    '*' -> (Token {literal = "*", _type = TIMES}, str)
    '/' -> (Token {literal = "/", _type = DIVIDE}, str)
    ch | isDigit ch -> readNumber (ch : str)
    ch -> (Token {literal = ch : "", _type = UNKNOWN}, str)

tokenize :: String -> [Token]
tokenize "" = []
tokenize str = 
    let result = nextToken str in
    fst result : tokenize (snd result)
