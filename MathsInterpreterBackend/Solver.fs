// solver.fs
namespace MathsInterpreterBackend

module Solver =

    open Lexer
    open Interpreter
    open Parser

    let equationError = System.Exception("Equation format error")

    // Split equation into left and right expressions, tokenize, and prepare for solving
    let splitAndTokenizeEquation (input: string) =
        let parts : string array = input.Split('=')
        if parts.Length <> 2 then raise equationError
        let leftTokens = lexer (parts.[0].Trim())
        let rightTokens = lexer (parts.[1].Trim())
        (leftTokens, rightTokens)


    // Evaluate each side of the equation for constant and variable components
    let rec evaluate tokens (constantSum, variableSum) =
        match tokens with
        | [] -> (constantSum, variableSum)
        | INTEGER value :: VARABLE _ :: tail -> evaluate tail (constantSum, variableSum + float value)
        | FLOAT value :: VARABLE _ :: tail -> evaluate tail (constantSum, variableSum + value)
        | INTEGER value :: tail -> evaluate tail (constantSum + float value, variableSum)
        | FLOAT value :: tail -> evaluate tail (constantSum + value, variableSum)
        | PLUS :: tail -> evaluate tail (constantSum, variableSum)
        | MINUS :: INTEGER value :: VARABLE _ :: tail -> evaluate tail (constantSum, variableSum - float value)
        | MINUS :: FLOAT value :: VARABLE _ :: tail -> evaluate tail (constantSum, variableSum - value)
        | MINUS :: INTEGER value :: tail -> evaluate tail (constantSum - float value, variableSum)
        | MINUS :: FLOAT value :: tail -> evaluate tail (constantSum - value, variableSum)
        | _ -> raise equationError

    // Calculate the solution based on evaluated components
    let solveEquation leftConstant leftVariableCoeff rightConstant rightVariableCoeff =
        let totalVariableCoeff = leftVariableCoeff - rightVariableCoeff
        let totalConstant = rightConstant - leftConstant
        if totalVariableCoeff = 0.0 then
            if totalConstant = 0.0 then "Infinite solutions"
            else "No solution"
        else
            let solution = totalConstant / totalVariableCoeff
            if solution = float (int solution) then
                $"x = {int solution}"
            else
                $"x = {solution:F2}"

    // Main function to solve linear equations
    let solveLinearEquation (input: string) : string =
        let leftTokens, rightTokens = splitAndTokenizeEquation input
        let leftConstant, leftVariableCoeff = evaluate leftTokens (0.0, 0.0)
        let rightConstant, rightVariableCoeff = evaluate rightTokens (0.0, 0.0)
        solveEquation leftConstant leftVariableCoeff rightConstant rightVariableCoeff
