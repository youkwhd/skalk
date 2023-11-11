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
            let number = head (tail tokens) in
            "    mov rax, " ++ literal number ++ "\n" ++ __parse (tail (tail tokens)) (_type function : LPAREN : operations)
        RPAREN ->
            assert (not (null operations))
            __parse tokens (tail (tail operations))
        NUMBER ->
            let op = peek operations in
            case op of
                -- Just MINUS ->
                --     "    ; negate number -- NOT IMPEMENTED YET\n" ++ __parse (tok : tokens) (tail operations)
                Just PLUS ->
                    "    add rax, " ++ literal tok ++ "\n" ++ __parse tokens operations
                Just TIMES ->
                    "    mul rax, " ++ literal tok ++ "\n" ++ __parse tokens operations
                _ ->
                    "    ; UNKNOWN OPERATION\n" ++ __parse tokens operations
        EOF -> assert (null operations) ""
        _ -> "    ; UNKOWN TOKEN\n" ++ __parse tokens operations
