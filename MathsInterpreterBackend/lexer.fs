// lexer.fs
namespace MathsInterpreterBackend

module Lexer = 

    type Token =
        | INTEGER of int
        | FLOAT of float
        | PLUS
        | MINUS
        | MULTIPLY
        | DIVIDE
        | REMAINDER
        | POWER 
        | LPAREN
        | RPAREN //adding power and brackets in 
        | PI 
        | VARABLE of char  //assing pi and letters into the tokens

    let lexError = System.Exception("Lexer error")

    let str2lst s = [for c in s -> c]
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let intVal (c:char) = (int)((int)c - (int)'0')

    //let rec scInt(iStr, iVal) = 
    //    match iStr with
    //    c :: tail when isDigit c -> scInt(tail, 10*iVal+(intVal c))
    //    | _ -> (iStr, iVal)

    let rec scFloat(iStr, iVal) =
        match iStr with
        | c :: tail when System.Char.IsDigit c -> scFloat(tail, iVal + string c)
        | '.' :: tail -> 
            let (rest, fracPart) = scFloat(tail, "")
            (rest, iVal + "." + fracPart)
        | _ -> (iStr, iVal)

    let isPi c = c = 'π'

    let isVarable c = 
    // Check if the character is a letter, excluding specific cases like π
        System.Char.IsLetter c && c <> 'π'


    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> []
            | '+' :: tail -> PLUS :: scan tail     
            | '-' :: tail -> MINUS :: scan tail    
            | '*' :: tail -> MULTIPLY :: scan tail
            | '/' :: tail -> DIVIDE :: scan tail
            | '^' :: tail -> POWER :: scan tail
            | '(' :: tail -> LPAREN :: scan tail
            | ')' :: tail -> RPAREN :: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c ->
                let (iStr, iVal) = scFloat(c :: tail, "")
                FLOAT (float iVal) :: scan iStr  // Always create FLOAT token
            | c :: tail when isPi c -> PI :: scan tail 
            | c :: tail when isVarable c -> VARABLE c :: scan tail
            | _ -> raise lexError  // Raise an error for unrecognized characters

        scan (str2lst input)

