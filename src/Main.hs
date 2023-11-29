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
        ["-e", prog] ->
            print (parse (tokenize prog)) 
        [file] ->
            -- TODO:
            -- read from file
            putStrLn ""
