// website i used https://fsharp.org/learn/ , https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/ , not the lectures cause they confuse me even more - Elise


// solver.fs
namespace MathsInterpreterBackend

module Solver =

    open Lexer
    open Interpreter
    open Parser

//    // Grammar in BNF:
//<EQ>   ::= <EX> "=" <EX>
//<EX>   ::= <TM> <EXO>
//<EXO>  ::= "+" <TM> <EXO> | "-" <TM> <EXO> | <empty>
//<TM>   ::= <FA> <TMO>
//<TMO>  ::= "*" <FA> <TMO> | "/" <FA> <TMO> | <empty>
//<FA>   ::= <BA> <FAO>
//<FAO>  ::= "^" <FA> | <empty>
//<BA>   ::= <NU> | <VA> | "(" <EX> ")"
//<NU>   ::= <IN> | <FL>
//<IN>   ::= <DI> <INO>
//<INO>  ::= <DI> <INO> | <empty>
//<FL>   ::= <IN> "." <IN>
//<VA>   ::= "x" | "y" | ...   // Any valid variable name
//<DI>   ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"





    let equationError = System.Exception("Equation format error")

    // Split the equation into two sides based on the '=' character
    let splitAndTokenizeEquation (input: string) =
        let parts : string array = input.Split('=')
        if parts.Length <> 2 then raise equationError
        let leftTokens = lexer (parts.[0].Trim())
        let rightTokens = lexer (parts.[1].Trim())
        (leftTokens, rightTokens)

    // Evaluate tokens to extract constant and variable components, identifying the variable symbol
    let rec evaluate tokens (constSum, varSum, varSymbol) =
        match tokens with
        | [] -> (constSum, varSum, varSymbol)
        | FLOAT value :: VARIABLE symbol :: tail ->
            let updatedSymbol = match varSymbol with
                                | None -> Some symbol
                                | Some existingSymbol when existingSymbol = symbol -> varSymbol
                                | Some _ -> raise equationError
            evaluate tail (constSum, varSum + value, updatedSymbol)
        | FLOAT value :: tail -> evaluate tail (constSum + value, varSum, varSymbol)
        | PLUS :: tail -> evaluate tail (constSum, varSum, varSymbol)
        | MINUS :: FLOAT value :: VARIABLE symbol :: tail ->
            let updatedSymbol = match varSymbol with
                                | None -> Some symbol
                                | Some existingSymbol when existingSymbol = symbol -> varSymbol
                                | Some _ -> raise equationError
            evaluate tail (constSum, varSum - value, updatedSymbol)
        | MINUS :: FLOAT value :: tail -> evaluate tail (constSum - value, varSum, varSymbol)
        | _ -> raise equationError

    // Calculate solution for the equation using identified constants and variable symbol
    let solveEquation leftConst leftVarCoeff rightConst rightVarCoeff varSymbol =
        let tVarCoeff = leftVarCoeff - rightVarCoeff
        let totalConst = rightConst - leftConst
        match varSymbol with
        | None -> raise equationError
        | Some symbol ->
            if tVarCoeff = 0.0 then
                if totalConst = 0.0 then "Infinite solutions"
                else "No solution"
            else
                let solution = totalConst / tVarCoeff
                if solution = float (int solution) then
                    $"{symbol} = {int solution}"
                else
                    $"{symbol} = {solution:F2}"

    // Main function to solve linear equations by parsing and evaluating both sides
    let solveLinearEquation (input: string) : string =
        let leftTokens, rightTokens = splitAndTokenizeEquation input
        let leftConstant, leftVariableCoeff, leftVarSymbol = evaluate leftTokens (0.0, 0.0, None)
        let rightConstant, rightVariableCoeff, rightVarSymbol = evaluate rightTokens (0.0, 0.0, None)
        let variableSymbol = leftVarSymbol |> Option.orElse rightVarSymbol
        solveEquation leftConstant leftVariableCoeff rightConstant rightVariableCoeff variableSymbol

