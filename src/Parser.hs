module Parser where

import Lexer
import Control.Exception

parse :: [Token] -> Int
parse tokens =  __parse tokens [] [] []

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:xs) = Just x

__parse :: [Token] -> [(TokenType, Int)] -> [TokenType] -> [Int] -> Int
__parse [] _ _ numbers = head numbers
__parse (tok:tokens) operations brackets numbers =
    case _type tok of
        LPAREN ->
            let op = head tokens in
            let before = peek operations in
            case before of
                Nothing ->
                    __parse (tail tokens) ((_type op, 0) : operations) (LPAREN : brackets) numbers
                Just x ->
                    __parse (tail tokens) ((_type op, 0) : (fst x, snd x + 1) : tail operations) (LPAREN : brackets) numbers
        RPAREN ->
            -- assert (not (null operations))
            -- assert (head brackets == LPAREN)
            let (op, n) = head operations in
            let num = case op of
                        PLUS -> foldr (+) 0 (reverse (take n numbers))
                        MINUS -> foldr (-) (head numbers) (reverse (take (n - 1) (tail numbers)))
                        TIMES -> foldr (*) 1 (reverse (take n numbers))
                        -- DIVIDE -> foldr (/) (head numbers) (reverse (take (n - 1) (tail numbers)))
                        _ -> 0 in
            __parse tokens (tail operations) (tail brackets) (num : drop n numbers)
        NUMBER ->
            let (op, n) = head operations in
            let _operations = tail operations in
            __parse tokens ((op, n + 1) : _operations) brackets ((read (literal tok) :: Int) : numbers)
        EOF -> __parse tokens operations brackets numbers
        _ -> 0
