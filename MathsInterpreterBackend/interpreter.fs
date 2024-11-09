// interpreter.fs
namespace MathsInterpreterBackend

module Interpreter =

    let addNumbers a b =
        match (a, b) with
        | (x, y) -> (x + y)


    let subtractNumbers a b =
        match (a, b) with
        | (x, y) -> (x - y)


    let multiplyNumbers a b =
        match (a, b) with
        | (x, y) -> (x * y)


    let divideNumbers a b =
        match (a, b) with
        | (x, y) -> (x / y)

            