﻿// lexer.fs
namespace MathsInterpreterBackend

open Interpreter

module Lexer = 

    type Token =
        | INTEGER of int
        | FLOAT of float
        | RATIONAL of Rational
        | PLUS
        | MINUS
        | MULTIPLY
        | DIVIDE
        | MODULO
        | FRACTION
        | REMAINDER
        | POWER
        | LPAREN
        | RPAREN //adding power and brackets in 
        | PI
        | EQUATION
        | VARIABLE of string  //assing pi and letters into the tokens
        | TYPEINT
        | TYPEFLOAT
        | TYPERATIONAL

    let lexError = System.Exception("Lexer error")

    let str2lst s = [for c in s -> c]
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let isChar  c = System.Char.IsLetter c           // Checks if character is a letter
    let intVal (c:char) = (int)((int)c - (int)'0')

    let rec scInt (iStr, iVal) = 
        match iStr with
        | c::tail when isDigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)

    let rec scFloat fStr fVal factor =
        match fStr with
        | c::tail when isDigit c -> 
            let newFVal = fVal + ((float (intVal c)) / factor)
            scFloat tail newFVal (factor * 10.0)
        | _ -> (fStr, fVal)

    // For parsing variable identidiers
    let rec scChar(iStr, vName:string) =
        match iStr with
        | c::tail when isChar c -> scChar(tail, vName + c.ToString())
        | _ -> (iStr, vName)


    let lexer (input: string) =
        let rec scan input =
            match input with
            | [] -> []
            | '+' :: tail -> PLUS :: scan tail     
            | '-' :: tail -> MINUS :: scan tail
            | '*' :: tail -> MULTIPLY :: scan tail
            | '÷' :: tail -> DIVIDE :: scan tail
            | '/' :: tail -> FRACTION :: scan tail
            | '%' :: tail -> MODULO :: scan tail
            | '^' :: tail -> POWER :: scan tail
            | '(' :: tail -> LPAREN :: scan tail
            | ')' :: tail -> RPAREN :: scan tail
            | '=' :: tail -> EQUATION :: scan tail
            | 'p' :: 'i' :: tail -> PI :: scan tail
            | 'π' :: tail -> PI :: scan tail
            | 'i' :: 'n' :: 't' :: ' ' :: tail -> TYPEINT :: scan tail
            | 'f' :: 'l' :: 'o' :: 'a' :: 't' :: ' ' :: tail -> TYPEFLOAT :: scan tail
            | 'r' :: 'a' :: 't' :: 'i' :: 'o' :: 'n' :: 'a' :: 'l' :: ' ' :: tail -> TYPERATIONAL :: scan tail
            | 'r' :: 'a' :: 't' :: ' ' :: tail -> TYPERATIONAL :: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c ->
                let (iStr, iVal) = scInt(tail, intVal c)
                match iStr with
                | '.' :: dTail ->
                    let (fStr, fVal) = scFloat dTail 0.0 10.0
                    FLOAT ((float iVal) + fVal) :: scan fStr
                | _ -> INTEGER iVal :: scan iStr
            | c :: tail when isChar c -> 
                let (iStr, vName) = scChar(tail, c.ToString())
                VARIABLE vName :: scan iStr
            | _ -> raise lexError  // Raise an error for unrecognized characters

        scan (str2lst input)

