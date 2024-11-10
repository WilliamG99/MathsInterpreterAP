// interpreter with f#
// Author: Elise & Alan
// Date: 09/11/24
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report 

// interpreter.fs
namespace MathsInterpreterBackend

module Interpreter =

    let piValue = System.Math.PI // Using F# System.Math for π

    let addNumbers (a: float) (b: float) : float =
        a + b

    let subtractNumbers (a: float) (b: float) : float =
        a - b

    let multiplyNumbers (a: float) (b: float) : float =
        a * b

    let divideNumbers (a: float) (b: float) : float =
        a / b

    let powerNumbers (a: float) (b: float) : float =
        System.Math.Pow(a, b)
