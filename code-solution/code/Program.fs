open Parser
open System
open Evaluator

[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn "Usage: please provide a .dot file"
        exit 1
    let file = args[0]
    if not (IO.File.Exists file) then
        printfn "Usage: File '%s' not found, please provide a .dot file" file
        exit 1
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        let svg = eval ast
        printfn "%s" svg
    | None ->
        printfn "Invalid program."
    0