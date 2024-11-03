// lexer.fs
namespace MathsInterpreterBackend

module Lexer = 

    type Token =
        | INTEGER of int
        | PLUS
        | MINUS
        | MULTIPLY
        | DIVIDE

    let lexError = System.Exception("Lexer error")

    let str2lst s = [for c in s -> c]
    let isDigit c = System.Char.IsDigit c

    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> []
            | '+' :: tail -> PLUS :: scan tail      // Handle the '+' operator
            | '-' :: tail -> MINUS :: scan tail     // Handle the '-' operator
            | '*' :: tail -> MULTIPLY :: scan tail  // Handle the '*' operator
            | '/' :: tail -> DIVIDE :: scan tail    // Handle the '/' operator
            | c :: tail when isDigit c ->           // Handle single-digit integers
                let value = int (string c)
                INTEGER(value) :: scan tail
            | _ -> raise lexError                   // Raise an error for unrecognized characters

        scan (str2lst input)