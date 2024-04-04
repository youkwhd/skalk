import Lexer
import Parser

import Control.Monad (forM, when)
import System.Environment (getArgs, getProgName)
import Text.Printf

-- Check if double is a fixed number
isFixed :: Double -> Bool
isFixed x = x == fromIntegral (truncate x)

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
        [] ->
            -- TODO:
            -- read from stdin
            printHelp progname
        ["-e", expr] ->
            let result = parse (tokenize expr) in
            if isFixed result then
                print (fromIntegral (truncate result))
            else
                print result
        ["-e"] ->
            printHelp progname
        [file] ->
            -- TODO:
            -- read from file
            printHelp progname
        _ ->
            printHelp progname
