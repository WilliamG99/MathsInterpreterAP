// parser.fs
namespace MathsInterpreterBackend

module Parser =

    open Lexer
    open Interpreter

    let parseError = System.Exception("Parser error")

    // Define a symbol table (variableName -> variableValue)
    let mutable symbolTable = Map.empty<string, Number> 
    type SymbolData = {
        Key: string
        Value: Number
        Type: string
    }

    // symbol list to store currently assigned variables with name, value & type
    let symbolList = 
        Map.toList symbolTable
        |> List.map (fun (k,v) ->
            let displayValue, valueType = 
                match v with
                | Int i -> i.ToString(), "Int"
                | Float f -> f.ToString(), "Float"
                | Rational r -> r.ToString(), "Rational"
                | Complex c -> c.ToString(), "Complex"
                | ComplexRational cr -> cr.ToString(), "Complex Rational"
            { Key = k; Value = v; Type = valueType }
        )

    let findVariable variableName =
        match Map.tryFind variableName symbolTable with
        | Some value -> value
        | None -> raise parseError


    let evaluateExpression tokenList =

        // Expression - addition & subtraction
        let rec parseExpression tokenList = (parseTerm >> parseExpressionOpt) tokenList
        and parseExpressionOpt (tokenList, value) =  // value is always float
            match tokenList with

            | PLUS :: tail ->
                let (remainingTokens, termValue) = parseTerm tail
                parseExpressionOpt (remainingTokens, addNumbers value termValue)

            | MINUS :: tail ->
                let (remainingTokens, termValue) = parseTerm tail
                parseExpressionOpt (remainingTokens, subtractNumbers value termValue)

            | _ -> (tokenList, value)

        // Term - multiplication & division
        and parseTerm tokenList = (parsePower >> parseTermOpt) tokenList
        and parseTermOpt (tokenList, value) =  // value is always float
            match tokenList with

            | MULTIPLY :: tail ->
                let (remainingTokens, termValue) = parsePower tail
                parseTermOpt (remainingTokens, multiplyNumbers value termValue)

            | DIVIDE :: tail ->
                let (remainingTokens, termValue) = parsePower tail
                parseTermOpt (remainingTokens, divideNumbers value termValue)

            | MODULO :: tail ->
                let (remainingTokens, termValue) = parsePower tail
                parseTermOpt (remainingTokens, moduloNumbers value termValue)

            | _ -> (tokenList, value)

        // Power - exponent operations
        and parsePower tokenList = (parseNumberOrParenthesis >> parsePowerOpt) tokenList
        and parsePowerOpt (tokenList, value) =  // value is always float
            match tokenList with

            | POWER :: tail ->
                let (remainingTokens, termValue) = parseNumberOrParenthesis tail
                parsePowerOpt (remainingTokens, powerNumbers value termValue)

            | _ -> (tokenList, value)

        // Numeric/Parenthesized/NumberTypes
        and parseNumberOrParenthesis tokenList =
            match tokenList with

            // Represebt Fraction as a Rational Number
            | INTEGER numerator :: FRACTION :: INTEGER denominator :: tail ->
                let newTail = RATIONAL {Numerator = numerator; Denominator = denominator} :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            // Represent Real and Imaginary Numbers as a Complex Number
            | LEFTPARENTHESIS :: INTEGER real :: PLUS :: INTEGER imaginary :: IMAGINARY :: RIGHTRPARENTHESIS :: tail ->
                let newTail = COMPLEX {Real = real; Imaginary = imaginary} :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            | LEFTPARENTHESIS :: INTEGER real :: MINUS :: INTEGER imaginary :: IMAGINARY :: RIGHTRPARENTHESIS :: tail ->
                let newTail = COMPLEX {Real = real; Imaginary = -(imaginary)} :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            // Range
            | TYPERANGE :: LEFTPARENTHESIS :: INTEGER val1 :: COMMA :: INTEGER val2 :: RIGHTRPARENTHESIS :: tail ->
                let newTail = RANGE {LowerBound = val1; UpperBound = val2} :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            // Plot
            | TYPEPLOT :: PLOT expression:: tail ->
                let (remainingTokens, termValue) = parseExpression tail
                (remainingTokens, termValue)


            // Implicit Multiplication
            | FLOAT value :: LEFTPARENTHESIS :: tail ->
                let newTail = FLOAT value :: MULTIPLY :: LEFTPARENTHESIS :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            | FLOAT value :: VARIABLE variableName :: tail ->
                let newTail = FLOAT value :: MULTIPLY :: VARIABLE variableName :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            | INTEGER value :: LEFTPARENTHESIS :: tail ->
                let newTail = INTEGER value :: MULTIPLY :: LEFTPARENTHESIS :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            | INTEGER value :: VARIABLE variableName :: tail ->
                let newTail = INTEGER value :: MULTIPLY :: VARIABLE variableName :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            | VARIABLE variableName :: LEFTPARENTHESIS :: tail ->
                let newTail = VARIABLE variableName :: MULTIPLY :: LEFTPARENTHESIS :: tail
                let (remainingTokens, termValue) = parseExpression newTail
                (remainingTokens, termValue)

            // Set value to its type
            | INTEGER value :: tail -> (tail, Int(value))
            | FLOAT value :: tail -> (tail, Float(value))
            | RATIONAL value :: tail -> (tail, Rational(value))
            | COMPLEX value :: tail -> (tail, Complex(value))
            | RANGE value :: tail -> (tail, Range(value))

            // Variable lookup
            | VARIABLE variableName :: tail ->
                let variableValue = findVariable variableName
                (tail, variableValue)

            // Number negation
            | MINUS :: tail ->
                let (remainingTokens, termValue) = parseNumberOrParenthesis  tail
                (remainingTokens, negateNumber termValue)

            // Implicit multiplication between parenthesies
            | LEFTPARENTHESIS :: tail -> 
                let (remainingTokens, leftval) = parseExpression tail
                match remainingTokens with
                | RIGHTRPARENTHESIS :: LEFTPARENTHESIS :: tail ->
                    let newTail = RIGHTRPARENTHESIS :: MULTIPLY :: LEFTPARENTHESIS :: tail
                    (newTail, leftval)
                | RIGHTRPARENTHESIS :: tail -> (tail, leftval)
                | _ -> raise parseError

            | PI :: tail -> (tail, Float(Interpreter.piValue))

            | _ -> raise parseError

        // Variable Assignment
        let parseVariableAssignment  tokenList =
            match tokenList with

            // Update the symbol table with the variable 
            | TYPEINT :: VARIABLE variableName :: EQUATION :: tail ->
                let (remainingTokens, termValue) = parseExpression tail
                match termValue with
                | Int x ->
                    symbolTable <- Map.add variableName termValue symbolTable
                    (remainingTokens, termValue)
                | Float x ->
                    let itval = Int(int x)
                    symbolTable <- Map.add variableName itval symbolTable
                    (remainingTokens, itval)
                | Rational x ->
                    raise (System.Exception("Cannot assign a Rational as an Integer"))

            | TYPEFLOAT :: VARIABLE variableName :: EQUATION :: tail ->
                let (remainingTokens, termValue) = parseExpression tail
                match termValue with
                | Float x -> 
                    symbolTable <- Map.add variableName termValue symbolTable
                    (remainingTokens, termValue)
                | Int x -> 
                    let ftval = Float(float x)
                    symbolTable <- Map.add variableName ftval symbolTable
                    (remainingTokens, ftval)
                | Rational x ->
                    raise (System.Exception("Cannot assign a Rational as an Float"))

            | TYPERATIONAL :: VARIABLE variableName :: EQUATION :: tail ->
                let (remainingTokens, termValue) = parseExpression tail
                match termValue with
                | Rational x ->
                    symbolTable <- Map.add variableName termValue symbolTable
                    (remainingTokens, termValue)
                | _ ->
                    raise (System.Exception("Can only assign a rational"))

            | TYPECOMPLEX :: VARIABLE variableName :: EQUATION :: tail ->
                let (remainingTokens, termValue) = parseExpression tail
                match termValue with
                | Complex x ->
                    symbolTable <- Map.add variableName termValue symbolTable
                    (remainingTokens, termValue)
                | ComplexRational x ->
                    symbolTable <- Map.add variableName termValue symbolTable
                    (remainingTokens, termValue)
                | _ ->
                    raise (System.Exception("Can only assign a complex"))

            | _ -> (parseExpression tokenList)
        parseVariableAssignment  tokenList

    let interpret (input: string) =
        let tokens = lexer input
        let _, result = evaluateExpression tokens
        match result with

        | Rational rational ->
            let newResult = rational.Numerator.ToString() + "/" + rational.Denominator.ToString()
            newResult

        | Complex complex ->
            let newResult = complex.Real.ToString() + " + " + complex.Imaginary.ToString() + "i"
            newResult

        | ComplexRational complexRational ->
            let newResult = complexRational.RealRational.Numerator.ToString() + "/" + complexRational.RealRational.Denominator.ToString() + complexRational.ImaginaryRational.Numerator.ToString() + "/" + complexRational.ImaginaryRational.Denominator.ToString() + "i"
            newResult

        | _ -> result.ToString()

    let getSymbolList () =
        Map.toList symbolTable
        |> List.map (fun (k, v) ->
            let displayValue, valueType =
                match v with
                | Int i -> i.ToString(), "Int"
                | Float f -> f.ToString(), "Float"
                | Rational r -> r.ToString(), "Rational"
                | Complex c -> c.ToString(), "Complex"
                | ComplexRational cr -> cr.ToString(), "ComplexRational"
            { Key = k; Value = v; Type = valueType }
        )

    let rec differentiate tokens =
        // Helper function to combine terms by exponent
        let simplify terms =
            let groupTerms terms =
                terms
                |> List.fold (fun acc (exp, coeff) ->
                    match Map.tryFind exp acc with
                    | Some existingCoeff -> Map.add exp (existingCoeff + coeff) acc
                    | None -> Map.add exp coeff acc
                ) Map.empty
            groupTerms terms
            |> Map.toList
            |> List.sortBy (fun (exp, _) -> -exp)
            |> List.map (fun (exp, coeff) ->
                match exp with
                | 0 -> sprintf "%d" coeff
                | 1 -> sprintf "%dx" coeff
                | _ -> sprintf "%dx^%d" coeff exp
            )
            |> String.concat " + "

        // Recursive helper function to differentiate tokens
        let rec differentiateHelper terms tokens sign =
            match tokens with
            | [] -> simplify terms, []

            | INTEGER c :: VARIABLE "x" :: POWER :: INTEGER n :: tail ->
                let newExponent = n - 1
                let newCoefficient = sign * c * n
                differentiateHelper ((newExponent, newCoefficient) :: terms) tail 1

            | INTEGER c :: VARIABLE "x" :: tail ->
                differentiateHelper ((0, sign * c) :: terms) tail 1

            | INTEGER c :: tail ->
                differentiateHelper ((0, 0) :: terms) tail 1

            | VARIABLE "x" :: POWER :: INTEGER n :: tail ->
                let newExponent = n - 1
                let coefficient = sign * n
                differentiateHelper ((newExponent, coefficient) :: terms) tail 1

            | VARIABLE "x" :: tail ->
                differentiateHelper ((0, sign * 1) :: terms) tail 1

            | PLUS :: tail ->
                differentiateHelper terms tail 1

            | MINUS :: tail ->
                differentiateHelper terms tail (-1)

            | _ -> raise (System.Exception("Unsupported token in differentiation"))

        differentiateHelper [] tokens 1