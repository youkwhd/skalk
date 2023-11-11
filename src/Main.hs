import Lexer
import Parser

import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    let prog = "(+ -6.2 2)" 
    let asm = "section .text\n\
              \    global main\n\
              \    extern printf\n\
              \\n\
              \exit:\n\
              \    mov rax, 0x3C\n\
              \    mov rdi, 0x00\n\
              \    syscall\n\
              \    ret\n\
              \\n\
              \main:\n"
              ++
              parse (tokenize prog)
              ++
              "    call exit\n" 
    putStr asm
