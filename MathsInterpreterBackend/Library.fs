namespace MathsInterpreterBackend

module Interpreter =
    let add a b = a + b
    let subtract a b = a - b


module Evaluator =
    let EvaluateExpression (expression: string) =
        // This would call your lexer/parser/interpreter logic
        // For now, a dummy example for testing:
        match expression with
        | "3+4" -> 7.0
        | _ -> failwith "Invalid expression"