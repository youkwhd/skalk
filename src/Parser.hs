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

_sum :: [Int] -> Int
_sum [] = 0
_sum (x:xs) = x + _sum xs

__parseEval :: [Token] -> TokenType -> Int -> (Int, [Token])
__parseEval (tok:tokens) op acc =
    case _type tok of
        LPAREN ->
            let r = __parse (tok : tokens) in
            trace ("ACC: -> " ++ show acc) $
            trace ("R: -> " ++ show (fst r)) $
            trace ("T: -> " ++ show (snd r)) $
            case op of
                PLUS -> __parseEval (snd r) op (acc + fst r)
                MINUS -> __parseEval (snd r) op (acc - fst r)
                TIMES -> __parseEval (snd r) op (acc * fst r)
        NUMBER ->
            let next  = head tokens in
            let val = read (literal tok) :: Int in
            case op of
                PLUS -> __parseEval tokens op (acc + val)
                MINUS -> __parseEval tokens op (acc - val)
                TIMES -> __parseEval tokens op (acc * val)
        _ ->
            (acc, tokens)

__parse :: [Token] -> (Int, [Token])
__parse (tok:tokens) =
    let op = head tokens in
    let firstNum = (read (literal (head (tail tokens))) :: Int) in
    __parseEval (tail (tail tokens)) (_type op) firstNum
