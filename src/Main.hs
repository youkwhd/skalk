import Lexer
import Parser

import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    let prog = "(* 5 (+ 10 10))"
    print (parse (tokenize prog))
