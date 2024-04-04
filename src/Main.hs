import Lexer
import Parser

import Control.Monad (forM, when)
import System.Environment (getArgs, getProgName)
import Text.Printf

-- Check if double is a fixed number
isFixed :: Double -> Bool
isFixed x = x == fromIntegral (truncate x)

interpretExprAndPrint :: String -> IO ()
interpretExprAndPrint expr =
    let result = parse (tokenize expr) in
    if isFixed result then
        print (fromIntegral (truncate result))
    else
        print result

printHelp :: String -> IO ()
printHelp progname = do
    putStrLn $ "Usage: " ++ progname ++ " [-e]"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName

    -- TODO:
    -- use getOpts
    case args of
        [] -> do
            expr <- getContents
            interpretExprAndPrint expr
        ["-e", expr] ->
            interpretExprAndPrint expr
        ["-e"] ->
            printHelp progname
        [filename] -> do
            expr <- readFile filename 
            interpretExprAndPrint expr
        _ ->
            printHelp progname
