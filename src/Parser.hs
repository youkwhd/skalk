module Parser where

import Lexer
import Data.Either
import Control.Exception

-- Check if double is a fixed number
isFixed :: Double -> Bool
isFixed x = x == fromIntegral (truncate x)

interpretExpr :: String -> Double
interpretExpr expr = parse (tokenize expr)

interpretExprAndPrint :: String -> IO ()
interpretExprAndPrint expr =
    let result = interpretExpr expr in
    if isFixed result then
        print (fromIntegral (truncate result))
    else
        print result

parse :: [Token] -> Double
parse tokens = fst (parseHead tokens)

parseHead :: [Token] -> (Double, [Token])
parseHead (tok:tokens) 
    | _type tok /= LPAREN = error "`tok` should be LPAREN"
    | otherwise =
        let op = head tokens in
        parseBodyFirstNum (tail tokens) (_type op)

parseBodyFirstNum :: [Token] -> TokenType -> (Double, [Token])
parseBodyFirstNum (tok:tokens) op 
    | _type tok == LPAREN = 
        let (result, rest) = parseHead (tok : tokens) in
        parseBody rest op 2 result
    | _type tok == NUMBER = parseBody tokens op 1 (read (literal tok) :: Double)
    | otherwise = error "unknown pattern"

parseBody :: [Token] -> TokenType -> Int -> Double -> (Double, [Token])
parseBody [] op iteration acc = (acc, [])
parseBody (tok:tokens) op iteration acc =
    case _type tok of
        LPAREN ->
            let (val, rest) = parseHead (tok : tokens) in
            case op of
                PLUS -> parseBody rest op (iteration + 1) (acc + val)
                MINUS -> parseBody rest op (iteration + 1) (acc - val)
                TIMES -> parseBody rest op (iteration + 1) (acc * val)
                DIVIDE -> parseBody rest op (iteration + 1) (acc / val)
        NUMBER ->
            let val = read (literal tok) :: Double in
            case op of
                PLUS -> parseBody tokens op (iteration + 1) (acc + val)
                MINUS -> parseBody tokens op (iteration + 1) (acc - val)
                TIMES -> parseBody tokens op (iteration + 1) (acc * val)
                DIVIDE -> parseBody tokens op (iteration + 1) (acc / val)
        RPAREN ->
            -- Edge case when the expression is a single number with the '-' (op)erator
            -- consider: (- 10), yields -10 (negation)
            let isNegation = iteration == 1 && op == MINUS in
            (if isNegation then -acc else acc, tokens)
        _ -> parseBody tokens op iteration acc
