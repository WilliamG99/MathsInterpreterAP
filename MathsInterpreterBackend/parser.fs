// parser.fs
namespace MathsInterpreterBackend

module Parser =

    open Lexer

    let parseError = System.Exception("Parser error")
    
    let ParseExpression (expression: string) =
        let tokens = Lexer.lexer expression
        match tokens with
        | [INTEGER a; PLUS; INTEGER b] -> float (Interpreter.add a b)
        | [INTEGER a; MINUS; INTEGER b] -> float (Interpreter.minus a b)
        | [INTEGER a; MULTIPLY; INTEGER b] -> float (Interpreter.multiply a b)
        | [INTEGER a; DIVIDE; INTEGER b] -> float (Interpreter.divide a b)
        | _ -> failwith "Invalid expression"