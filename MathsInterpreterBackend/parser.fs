// parser.fs
namespace MathsInterpreterBackend

module Parser =

    open Lexer
    open Interpreter

    let parseError = System.Exception("Parser error")
    

    let evaluateExpression tList =
        // Expression - addition & subtraction
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, value) = 
            match tList with
            | PLUS  :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, addNumbers value tval)
            | MINUS :: tail ->
                let (tLst, tval) = T tail
                Eopt (tLst, subtractNumbers value tval)
            | _ -> (tList, value)

        and T tList = (NR >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | MULTIPLY :: tail ->
                let (tLst, tval) = T tail
                Topt (tLst, multiplyNumbers value tval)
            | DIVIDE :: tail ->
                let (tLst, tval) = T tail
                Topt (tLst, divideNumbers value tval)
            | _ -> (tList, value)

        and NR tList =
            match tList with
            | INTEGER value :: tail -> (tail, value)
            | LPAREN :: tail -> let (tLst, tval) = E tail
                                match tLst with 
                                | RPAREN :: tail -> (tail, tval)
                                | _ -> raise parseError
            | _ -> raise parseError
        E tList

    let interpret input =
        let tokens = lexer input
        let _, result = evaluateExpression tokens
        result