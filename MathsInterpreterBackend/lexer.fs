// lexer.fs
namespace MathsInterpreterBackend

open Interpreter

module Lexer = 

    type Token =
        | INTEGER of int
        | FLOAT of float
        | RATIONAL of Rational
        | COMPLEX of Complex
        | PLUS
        | MINUS
        | MULTIPLY
        | DIVIDE
        | MODULO
        | FRACTION              // For rationals
        | POWER
        | IMAGINARY             // For complex numbers
        | LEFTPARENTHESIS
        | RIGHTRPARENTHESIS
        | PI
        | EQUATION
        | VARIABLE of string  //assinge pi and letters into the tokens
        | TYPEINT
        | TYPEFLOAT
        | TYPERATIONAL
        | TYPECOMPLEX

        | TYPERANGE
        | RANGE of Range
        | COMMA

        | TYPEPLOT
        | PLOT of string

    let lexError = System.Exception("Lexer error")

    let stringToCharList  s = [for c in s -> c]
    let isWhitespace c = System.Char.IsWhiteSpace c
    let isNumeric  c = System.Char.IsDigit c
    let isChar  c = System.Char.IsLetter c
    let charToInt ch = int ch - int '0'

    let rec parseInteger (iStr, iVal) = 
        match iStr with
        | c::tail when isNumeric c -> parseInteger(tail, 10*iVal+(charToInt c))
        | _ -> (iStr, iVal)

    let rec parseFloat fStr fVal factor =
        match fStr with
        | c::tail when isNumeric c -> 
            let newFVal = fVal + ((float (charToInt c)) / factor)
            parseFloat tail newFVal (factor * 10.0)
        | _ -> (fStr, fVal)

    // For parsing variable identidiers
    let rec parseChar(iStr, vName:string) =
        match iStr with
        | c::tail when isChar c -> parseChar(tail, vName + c.ToString())
        | _ -> (iStr, vName)


    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> []
            | '+' :: tail -> PLUS :: scan tail     
            | '-' :: tail -> MINUS :: scan tail
            | '*' :: tail -> MULTIPLY :: scan tail
            | '÷' :: tail -> DIVIDE :: scan tail
            | '%' :: tail -> MODULO :: scan tail
            | '^' :: tail -> POWER :: scan tail
            | '/' :: tail -> FRACTION :: scan tail
            | '(' :: tail -> LEFTPARENTHESIS :: scan tail
            | ')' :: tail -> RIGHTRPARENTHESIS :: scan tail
            | '=' :: tail -> EQUATION :: scan tail

            | 'p' :: 'i' :: tail -> PI :: scan tail
            | 'π' :: tail -> PI :: scan tail
            | 'i' :: 'n' :: 't' :: ' ' :: tail -> TYPEINT :: scan tail
            | 'f' :: 'l' :: 'o' :: 'a' :: 't' :: ' ' :: tail -> TYPEFLOAT :: scan tail
            | 'r' :: 'a' :: 't' :: 'i' :: 'o' :: 'n' :: 'a' :: 'l' :: ' ' :: tail -> TYPERATIONAL :: scan tail
            | 'r' :: 'a' :: 't' :: ' ' :: tail -> TYPERATIONAL :: scan tail
            | 'c' :: 'o' :: 'm' :: 'p' :: 'l' :: 'e' :: 'x' :: ' ' :: tail -> TYPECOMPLEX :: scan tail
            | 'c' :: 'o' :: 'm' :: ' ' :: tail -> TYPECOMPLEX :: scan tail
            | 'i' :: tail -> IMAGINARY :: scan tail

            | 'r' :: 'a' :: 'n' :: 'g' :: 'e' :: tail -> TYPERANGE :: scan tail
            | ',' :: tail -> COMMA :: scan tail

            | 'p' :: 'l' :: 'o' :: 't' :: tail -> TYPEPLOT :: scan tail

            | c :: tail when isWhitespace c -> scan tail
            | c :: tail when isNumeric c ->
                let (remainingString, intVal) = parseInteger(tail, charToInt c)
                match remainingString with
                | '.' :: dTail ->
                    let (fStr, fVal) = parseFloat dTail 0.0 10.0
                    FLOAT ((float intVal) + fVal) :: scan fStr
                | _ -> INTEGER intVal :: scan remainingString
            | c :: tail when isChar c -> 
                let (remainingString, varviableName) = parseChar(tail, c.ToString())
                VARIABLE varviableName :: scan remainingString
            | _ -> raise lexError  // Raise an error for unrecognized characters

        scan (stringToCharList  input)

    let tokenToString token =
        match token with
        | INTEGER n -> string n
        | FLOAT f -> string f
        | VARIABLE v -> v
        | PLUS -> "+"
        | MINUS -> "-"
        | MULTIPLY -> "*"
        | DIVIDE -> "/"
        | POWER -> "^"
        | LEFTPARENTHESIS -> "("
        | RIGHTRPARENTHESIS -> ")"
        | _ -> "?"

