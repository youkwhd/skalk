import Lexer
import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    mapM_ (\tok -> do print (_type tok); print (literal tok)) (tokenize "(+ -6.2 2)")
