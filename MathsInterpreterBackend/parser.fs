// parser.fs
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
                | Rational r -> r.ToString(), "Rational"
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
            | MODULO :: tail ->
                let (tLst, tval) = P tail
                Topt (tLst, moduloNumbers value tval)
            | _ -> (tList, value)

        // Power - exponent operations
        and P tList = (NR >> Popt) tList
        and Popt (tList, value) =  // value is always float
            match tList with
            | POWER :: tail ->
                let (tLst, tval) = NR tail
                Popt (tLst, powerNumbers value tval)
            | _ -> (tList, value)

        // Numeric/Parenthesized/NumberTypes
        and NR tList =
            match tList with

            // Represebt Fraction as a Rational Number
            | INTEGER numerator :: FRACTION :: INTEGER denominator :: tail ->
                let newTail = RATIONAL {Numerator = numerator; Denominator = denominator} :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)

            // Represent Real and Imaginary Numbers as a Complex Number
            | LPAREN :: INTEGER real :: PLUS :: INTEGER imaginary :: IMAGINARY :: RPAREN :: tail ->
                let newTail = COMPLEX {Real = real; Imaginary = imaginary} :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)
            | LPAREN :: INTEGER real :: MINUS :: INTEGER imaginary :: IMAGINARY :: RPAREN :: tail ->
                let newTail = COMPLEX {Real = real; Imaginary = -(imaginary)} :: tail
                let (tLst, tval) = E newTail
                (tLst, tval)


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
            | RATIONAL value :: tail -> (tail, Rational(value))
            | COMPLEX value :: tail -> (tail, Complex(value))
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

        // Variable Assignment
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
                | Rational x ->
                    raise (System.Exception("Cannot assign a Rational as an Integer"))
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
                | Rational x ->
                    raise (System.Exception("Cannot assign a Rational as an Float"))
            | TYPERATIONAL :: VARIABLE vName :: EQUATION :: tail ->
                let (tLst, tval) = E tail
                match tval with
                | Rational x ->
                    symbolTable <- Map.add vName tval symbolTable
                    (tLst, tval)
                | _ ->
                    raise (System.Exception("Can only assign a rational"))

            | _ -> (E tList)
        VA tList

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
        | _ ->
            result.ToString()



    let getSymbolList () =
        Map.toList symbolTable
        |> List.map (fun (k, v) ->
            let displayValue, valueType =
                match v with
                | Int i -> i.ToString(), "Int"
                | Float f -> f.ToString(), "Float"
                | Rational r -> r.ToString(), "Rational"
            { Key = k; Value = v; Type = valueType }
        )




