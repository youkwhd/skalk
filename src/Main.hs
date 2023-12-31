import Lexer
import Parser

import Control.Monad (forM, when)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName

    -- TODO:
    -- use getOpts
    case args of
        [] ->
            putStrLn progname
        ["-e", expr] ->
            print (parse (tokenize expr)) 
        [file] ->
            -- TODO:
            -- read from file
            putStrLn ""
