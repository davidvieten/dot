open Parser
open System
open Evaluator
open System.Diagnostics 

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage: please provide a .dot file."
        exit 1
    let file = args[0]
    if args.Length > 2 then 
        printfn "Usage: Too many arguments. Please only provide a .dot file and optionally, a new filename for the resulting SVG."
        exit 1
    if not (IO.File.Exists file) then
        printfn "Usage: File '%s' not found, please provide a .dot file to produce an SVG" file
        exit 1
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        let svg = eval ast
        if args.Length > 1 then
            let newFile = args[1]
            //changes newFile to svg format
            let outputFile = IO.Path.ChangeExtension(newFile, ".svg") 
            //writes the svg output to the new file
            IO.File.WriteAllText(outputFile, svg)  
            //
            let processInfo = new ProcessStartInfo(outputFile)  
            //allows us to use the shell which will be used to open the .svg file using the default application
            processInfo.UseShellExecute <- true
            //attempts to open the .svg in default application
            Process.Start(processInfo) |> ignore //discards the return value from start
        else 
            printfn "%s" svg
    | None ->
        printfn "Invalid program."
    0