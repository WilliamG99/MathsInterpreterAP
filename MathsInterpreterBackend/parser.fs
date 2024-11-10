// parser.fs
namespace MathsInterpreterBackend

module Parser =

    open Lexer
    open Interpreter

    let parseError = System.Exception("Parser error")

    let evaluateExpression tList =
        // Expression - addition & subtraction
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, value) =  // value is always float
            match tList with
            | PLUS :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, addNumbers value tval)
            | MINUS :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, subtractNumbers value tval)
            | _ -> (tList, value)

        // Term - multiplication & division
        and T tList = (P >> Topt) tList
        and Topt (tList, value) =  // value is always float
            match tList with
            | MULTIPLY :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, multiplyNumbers value tval)
            | DIVIDE :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, divideNumbers value tval)
            | _ -> (tList, value)

        // Power - exponent operations
        and P tList = (NR >> Popt) tList
        and Popt (tList, value) =  // value is always float
            match tList with
            | POWER :: tail ->
                let (tLst, tval) = NR tail
                Popt (tLst, powerNumbers value tval)
            | _ -> (tList, value)

        // Numeric/Parenthesized
        and NR tList =
            match tList with
            | INTEGER value :: tail -> (tail, float value)  // Convert INTEGER to float immediately
            | FLOAT value :: tail -> (tail, value)          // FLOAT is already float
            | LPAREN :: tail -> 
                let (tLst, tval) = E tail
                match tLst with 
                | RPAREN :: tail -> (tail, tval)
                | _ -> raise parseError
            | PI :: tail -> (tail, Interpreter.piValue)     // Use piValue directly
            | _ -> raise parseError

        E tList

    let interpret (input: string) : float =
        let tokens = lexer input
        let _, result = evaluateExpression tokens
        result

