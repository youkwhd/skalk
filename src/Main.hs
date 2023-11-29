import Lexer
import Parser

import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    let prog = "(- 10 20 (* 2 2 5))"
    let r = __parse (tokenize prog)
    putStr "RESULT: "
    print (fst r)
    mapM_ (\tok -> do putStrLn ""; print (_type tok); print (literal tok)) (snd r)
