import Lexer
import Parser

import Control.Monad (forM, when)
import System.Environment (getArgs, getProgName)
import Distribution.Compat.Prelude (exitFailure, exitSuccess)

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
        ["-h"] -> printHelp progname
        ["--help"] -> printHelp progname
        ["-e"] -> do
            printHelp progname
            exitFailure
        ["-e", expr] ->
            interpretExprAndPrint expr
        [filename] -> do
            expr <- readFile filename 
            interpretExprAndPrint expr
        _ -> do
            printHelp progname
            exitFailure
