module Parser where

import Lexer
import Control.Exception

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:xs) = Just x

parse :: [Token] -> String
parse tokens =  __parse tokens []

-- [TokenType] stack of token types
__parse :: [Token] -> [TokenType] -> String
__parse [] operations = ""
__parse (tok:tokens) operations =
    case _type tok of
        LPAREN -> 
            let function = head tokens in
            __parse (tail tokens) (LPAREN : operations)
        RPAREN ->
            assert (not (null operations))
            __parse tokens (init operations)
        MINUS -> __parse tokens (MINUS : operations)
        NUMBER ->
            let op = peek operations in
            if op == Just MINUS then
                "    ; negate number -- NOT IMPEMENTED YET\n" ++ __parse (tok : tokens) (tail operations)
            else
                "    ; mov number\n" ++ __parse tokens operations
        EOF -> assert (null operations) ""
        _ -> "    ; UNKOWN TOKEN\n" ++ __parse tokens operations
