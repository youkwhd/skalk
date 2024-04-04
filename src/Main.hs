import Lexer
import Parser

import Control.Monad (forM, when)
import System.Environment (getArgs, getProgName)

printHelp :: String -> IO ()
printHelp progname = do
    putStrLn $ "Usage: " ++ progname ++ " -e"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName

    -- TODO:
    -- use getOpts
    case args of
        [] ->
            printHelp progname
        ["-e", expr] ->
            print (parse (tokenize expr)) 
        [file] ->
            -- TODO:
            -- read from file
            printHelp progname
