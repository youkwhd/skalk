module Parser where

import Lexer
import Debug.Trace
import Data.Either
import Control.Exception

parse :: [Token] -> Int
parse tokens = fst (__parse tokens)

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:xs) = Just x

__parseEval :: [Token] -> TokenType -> Int -> Int -> (Int, [Token])
__parseEval [] op iteration acc = (acc, [])
__parseEval (tok:tokens) op iteration acc =
    case _type tok of
        NUMBER ->
            let val = read (literal tok) :: Int in
            case op of
                PLUS -> __parseEval tokens op (iteration + 1) (acc + val)
                MINUS -> __parseEval tokens op (iteration + 1) (acc - val)
                TIMES -> __parseEval tokens op (iteration + 1) (acc * val)
        LPAREN ->
            let result = __parse (tok : tokens) in
            case op of
                PLUS -> __parseEval (snd result) op (iteration + 1) (acc + fst result)
                MINUS -> __parseEval (snd result) op (iteration + 1) (acc - fst result)
                TIMES -> __parseEval (snd result) op (iteration + 1) (acc * fst result)
        RPAREN ->
            let isNegation = iteration == 0 && op == MINUS in
            (if isNegation then -acc else acc, tokens)
        _ -> __parseEval tokens op iteration acc

__parse :: [Token] -> (Int, [Token])
__parse (tok:tokens) =
    let op = head tokens in
    case _type (head (tail tokens)) of
        NUMBER ->
            let firstNum = (read (literal (head (tail tokens))) :: Int) in
            __parseEval (tail (tail tokens)) (_type op) 0 firstNum
        LPAREN ->
            let result = __parse (tail tokens) in
            __parseEval (snd result) (_type op) 0 (fst result)
