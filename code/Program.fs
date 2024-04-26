open Parser
open System

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        printfn "%A" ast
    | None ->
        printfn "Invalid program."
    0