// lexer.fs
namespace MathsInterpreterBackend

module Lexer = 

    type Token =
        | INTEGER of int
        | PLUS
        | MINUS
        | MULTIPLY
        | DIVIDE
        | REMAINDER
        | POWER
        | LPAREN
        | RPAREN

    let lexError = System.Exception("Lexer error")

    let str2lst s = [for c in s -> c]
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let intVal (c:char) = (int)((int)c - (int)'0')

    let rec scInt(iStr, iVal) = 
        match iStr with
        c :: tail when isDigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)

    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> []
            | '+' :: tail -> PLUS :: scan tail     
            | '-' :: tail -> MINUS :: scan tail    
            | '*' :: tail -> MULTIPLY :: scan tail
            | '/' :: tail -> DIVIDE :: scan tail
            | '^' :: tail -> POWER :: scan tail
            | '('::tail -> LPAREN:: scan tail
            | ')'::tail -> RPAREN:: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c ->
                let (iStr, iVal) = scInt(tail, intVal c)
                INTEGER iVal :: scan iStr
            | _ -> raise lexError                   // Raise an error for unrecognized characters

        scan (str2lst input)