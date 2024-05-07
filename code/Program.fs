open Parser
open System
open Evaluator

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        //printfn "%A" ast
        let svg = eval ast
        printfn "%s" svg
    | None ->
        printfn "Invalid program."
    0