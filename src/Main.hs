import Lexer
import Parser

import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    let prog = "(+ 1 1 (- 1))"
    let (result, tokens) = __parse (tokenize prog)
    print result
