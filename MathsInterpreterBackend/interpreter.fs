// Interpreter.fs
namespace MathsInterpreterBackend


open System
open Lexer
open Parser

module Interpreter =

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []


    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        let input:string = getInputString()
        let oList = lexer input
        let sList = printTList oList;
        let pList = printTList (parser oList)
        let Out = parseNeval oList
        Console.WriteLine("Result = {0}", snd Out)
        0
