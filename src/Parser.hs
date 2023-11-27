module Parser where

import Lexer
import Control.Exception

parse :: [Token] -> Int
parse tokens = fst (__parse tokens PLUS)

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:xs) = Just x

__parse :: [Token] -> TokenType -> (Int, [Token])
__parse [] _ = (0, [])
__parse (tok:tokens) op =
    case _type tok of
        RPAREN ->
            (0, [])
        LPAREN ->
            __parse tokens op
        NUMBER ->
            let next_token = head tokens in

            if _type next_token == RPAREN then
                (read (literal tok) :: Int, tokens)
            else
                let r = __parse tokens op in
                case op of
                    PLUS ->
                        ((read (literal tok) :: Int) + fst r, snd r)
                    MINUS ->
                        ((read (literal tok) :: Int) - fst r, snd r)
                    TIMES ->
                        ((read (literal tok) :: Int) * fst r, snd r)
        PLUS ->
            __parse tokens PLUS
        MINUS ->
            __parse tokens MINUS
        TIMES ->
            __parse tokens TIMES
