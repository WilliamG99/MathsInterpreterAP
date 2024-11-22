﻿// parser.fs
namespace MathsInterpreterBackend

module Parser =

    open Lexer
    open Interpreter



    // Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"


    let parseError = System.Exception("Parser error")


    // Define a symbol table (variableName -> variableValue)
    let mutable symbolTable = Map.empty<string, Number> 
    type SymbolData = {
        Key: string
        Value: Number
        Type: string
    }

    // symbolList to store currently assigned variables with name, value & type
    let symbolList = 
        Map.toList symbolTable |> List.map (fun (k,v) ->
            let displayValue, valueType = 
                match v with
                | Int i -> i.ToString(), "Int"
                | Float f -> f.ToString(), "Float"
            { Key = k; Value = v; Type = valueType }
        )

    // Function to look up variable values
    let lookupVariable variableName =
        match Map.tryFind variableName symbolTable with
        | Some value -> value
        | None -> raise parseError


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

            // Implicit Multiplication
            | FLOAT value :: LPAREN :: tail ->
                let newTail = FLOAT value :: MULTIPLY :: LPAREN :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | FLOAT value :: VARIABLE vName :: tail ->
                let newTail = FLOAT value :: MULTIPLY :: VARIABLE vName :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | INTEGER value :: LPAREN :: tail ->
                let newTail = INTEGER value :: MULTIPLY :: LPAREN :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | INTEGER value :: VARIABLE vName :: tail ->
                let newTail = INTEGER value :: MULTIPLY :: VARIABLE vName :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | VARIABLE vName :: LPAREN :: tail ->
                let newTail = VARIABLE vName :: MULTIPLY :: LPAREN :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)

            | INTEGER value :: tail -> (tail, Int(value))
            | FLOAT value :: tail -> (tail, Float(value))
            | VARIABLE vName :: tail ->
                let variableValue = lookupVariable vName
                (tail, variableValue)
            | MINUS :: tail ->
                let (tLst, tval) = NR tail
                (tLst, negateNumber tval)
            | LPAREN :: tail -> 
                let (tLst, leftval) = E tail
                match tLst with
                | RPAREN :: LPAREN :: tail ->
                    let newTail = RPAREN :: MULTIPLY :: LPAREN :: tail
                    (newTail, leftval)
                | RPAREN :: tail -> (tail, leftval)
                | _ -> raise parseError
            | PI :: tail -> (tail, Float(Interpreter.piValue))     // Use piValue directly
            | _ -> raise parseError

        // Variable Assignment & for loop expressions
        let VA tList =
            match tList with
            // Update the symbol table with the variable 
            | TYPEINT :: VARIABLE vName :: EQUATION :: tail ->
                let (tLst, tval) = E tail
                match tval with
                | Int x ->
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)
                | Float x ->
                    let itval = Int(int x)
                    symbolTable <- Map.add vName itval symbolTable
                    (tLst, itval)
            | TYPEFLOAT :: VARIABLE vName :: EQUATION :: tail ->
                let (tLst, tval) = E tail
                match tval with
                | Float x -> 
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)
                | Int x -> 
                    let ftval = Float(float x)
                    symbolTable <- Map.add vName ftval symbolTable
                    (tLst, ftval)
            | _ -> (E tList)
        VA tList

    let interpret (input: string) =
        let tokens = lexer input
        let _, result = evaluateExpression tokens
        result






