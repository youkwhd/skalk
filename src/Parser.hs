module Parser where

import Lexer
import Debug.Trace
import Data.Either
import Control.Exception

parse :: [Token] -> Int
parse tokens = fst (parseHead tokens)

parseHead :: [Token] -> (Int, [Token])
parseHead (tok:tokens) =
    let op = head tokens in
    case _type (head (tail tokens)) of
        NUMBER ->
            let firstNum = (read (literal (head (tail tokens))) :: Int) in
            parseBody (tail (tail tokens)) (_type op) 0 firstNum
        LPAREN ->
            let result = parseHead (tail tokens) in
            parseBody (snd result) (_type op) 0 (fst result)

parseBody :: [Token] -> TokenType -> Int -> Int -> (Int, [Token])
parseBody [] op iteration acc = (acc, [])
parseBody (tok:tokens) op iteration acc =
    case _type tok of
        NUMBER ->
            let val = read (literal tok) :: Int in
            case op of
                PLUS -> parseBody tokens op (iteration + 1) (acc + val)
                MINUS -> parseBody tokens op (iteration + 1) (acc - val)
                TIMES -> parseBody tokens op (iteration + 1) (acc * val)
        LPAREN ->
            let result = parseHead (tok : tokens) in
            case op of
                PLUS -> parseBody (snd result) op (iteration + 1) (acc + fst result)
                MINUS -> parseBody (snd result) op (iteration + 1) (acc - fst result)
                TIMES -> parseBody (snd result) op (iteration + 1) (acc * fst result)
        RPAREN ->
            let isNegation = iteration == 0 && op == MINUS in
            (if isNegation then -acc else acc, tokens)
        _ -> parseBody tokens op iteration acc
