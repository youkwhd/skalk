import Lexer
import Parser

import Data.Char (isDigit)
import Control.Monad (forM)

main :: IO ()
main = do
    let prog = "(+ 6 2)" 
    let asm = "section .data\n\
              \    fmt: db \"%d\", 0x0A, 0x0D, 0x00\n\
              \\n\
              \section .text\n\
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
              "\n    mov rdi, fmt\n\
              \    mov rsi, rax\n\
              \    mov eax, 0x00\n\
              \    call printf\n\
              \    call exit\n" 
    putStr asm
