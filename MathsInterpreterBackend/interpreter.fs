// interpreter with f#
// Author: Elise & Alan
// Date: 09/11/24
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report 

// interpreter.fs
namespace MathsInterpreterBackend

module Interpreter =

    let piValue = System.Math.PI // Using F# System.Math for π

    type Number =
    | Int of int
    | Float of float

    let addNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x + y)
        | (Float x, Float y) -> Float (x + y)
        | (Int x, Float y) -> Float ((float x) + y)
        | (Float x, Int y) -> Float (x + (float y))

    let subtractNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x - y)
        | (Float x, Float y) -> Float (x - y)
        | (Int x, Float y) -> Float ((float x) - y)
        | (Float x, Int y) -> Float (x - (float y))

    let multiplyNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x * y)
        | (Float x, Float y) -> Float (x * y)
        | (Int x, Float y) -> Float ((float x) * y)
        | (Float x, Int y) -> Float (x * (float y))

    let divideNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (x / y)
        | (Float x, Float y) -> Float (x / y)
        | (Int x, Float y) -> Float ((float x) / y)
        | (Float x, Int y) -> Float (x / (float y))

    let powerNumbers a b =
        match (a, b) with
        | (Int x, Int y) -> Int (System.Convert.ToInt32(System.Math.Pow(float x, float y)))
        | (Float x, Float y) -> Float (System.Math.Pow(x, y))
        | (Int x, Float y) -> Float (System.Math.Pow((float x), y))
        | (Float x, Int y) -> Float (System.Math.Pow(x, (float y)))

    let negateNumber a =
        match a with
        | Int x -> Int (-x)
        | Float x -> Float (-x)

