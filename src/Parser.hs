module Parser where

import Lexer
import Data.Either
import Control.Exception

parse :: [Token] -> Int
parse tokens = fst (__parse tokens)

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:xs) = Just x

__parseEval :: [Token] -> TokenType -> (Int, [Token])
__parseEval (tok:tokens) op =
    case _type tok of
        LPAREN ->
            let r = __parse (tok : tokens) in
            let _r = __parseEval (snd r) op in
            case op of
                PLUS -> (fst _r + fst r, snd _r)
                MINUS -> (fst _r - fst r, snd _r)
                TIMES -> (fst _r * fst r, snd _r)
        RPAREN ->
            (0, [])
        _ ->
            let next  = head tokens in
            case _type next of
                RPAREN ->
                    let val = read (literal tok) :: Int in
                    (val, tail tokens)
                _ ->
                    let r = __parseEval tokens op in
                    let val = read (literal tok) :: Int in
                    case op of
                        PLUS -> (val + fst r, snd r)
                        MINUS -> (val - fst r, snd r)
                        TIMES -> (val * fst r, snd r)


__parse :: [Token] -> (Int, [Token])
__parse (tok:tokens) =
    let op = head tokens in
    __parseEval (tail tokens) (_type op)
