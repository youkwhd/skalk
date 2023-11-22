import Lexer
import Parser

import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    let prog = "(+ 5 (* 2 4) (+ 1 1))" 
    print (parse (tokenize prog))
