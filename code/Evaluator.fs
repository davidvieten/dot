module Evaluator

open AST

// let evalRoute (route: Route) : string =
//     "  <line x1=\"" + (route.start.x |> string) + "\"" +
//     " y1=\"" + (route.start.y |> string) + "\"" +
//     " x2=\"" + (route.endRoute.x |> string) + "\"" +
//     " y2=\"" + (route.endRoute.y |> string) + "\"" +
//     " style=\"stroke:" +
//     (evalColor line.color) + ";stroke-width:2\" />\n"

// let rec evalCanvas (canvas: Canvas) : string =
//     match canvas with
//     | [] -> ""
//     | l::ls -> (evalLine l) + (evalCanvas ls)

// let eval (canvas: Canvas) : string =
//     let csz = CANVAS_SZ |> string
//     "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
//     " xmlns=\"http://www.w3.org/2000/svg\"" +
//     " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
//     (evalCanvas canvas)
//     + "</svg>\n"