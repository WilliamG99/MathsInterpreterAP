// Lexer.fs
namespace MathsInterpreterBackend

module Lexer =

    open System

    type terminal = 
        | Add 
        | Sub 
        | Mul 
        | Div 
        | Lpar 
        | Rpar 
        | Num of int

    let lexError = System.Exception("Lexer error")

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let intVal (c:char) = (int)((int)c - (int)'0')

    let rec scInt (iStr, iVal) = 
        match iStr with
        | c :: tail when isdigit c -> scInt(tail, 10 * iVal + intVal c)
        | _ -> (iStr, iVal)

    let lexer input = 
        let rec scan input =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail
            | '/'::tail -> Div :: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c -> 
                let (iStr, iVal) = scInt(tail, intVal c) 
                Num iVal :: scan iStr
            | _ -> raise lexError
        scan (str2lst input)
