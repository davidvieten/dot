module Evaluator

open AST


let evalRoute (start: Location)(endRoute: EndRoute) : string =
    let startX, startY =
        match start with
        | LeftHash c -> c.x, c.y
        | RightHash c -> c.x, c.y
        | Dot c -> c.x, c.y
        | RightPoint c -> c.x, c.y
        | LeftPoint c -> c.x, c.y
        | StackInside c -> c.x, c.y
        | StackOutside c -> c.x, c.y
    let endX, endY =
        match endRoute with
        | Net c -> c.x, c.y
        | WalkLine c -> c.x, c.y
        | DownWall c -> c.x, c.y
        | UpWall c -> c.x, c.y
        | Corner c -> c.x, c.y
        | Slot c -> c.x, c.y
        | BackDoor c -> c.x, c.y
    "<circle cx=\"" + (startX |> string) + "\"" +
    " cy=\"" + (startY |> string) + "\"" +
    " r=\"5\" fill=\"black\" />\n" +
    "<circle cx=\"" + (endX |> string) + "\"" +
    " cy=\"" + (endY |> string) + "\"" +
    " r=\"5\" fill=\"black\" />\n" +
    "  <line x1=\"" + (startX |> string) + "\"" +
    " y1=\"" + (startY |> string) + "\"" +
    " x2=\"" + (endX|> string) + "\"" +
    " y2=\"" + (endY|> string) + "\"" +
    " style=\"stroke:black;stroke-width:3\" />\n"
    

let rec evalBoard (board: Board) : string =
    match board with
    | [] -> ""
    | { start = ((_, location), _); endRoute = endRoute }::ls -> (evalRoute location endRoute) + (evalBoard ls)

    
let eval (board: Board) : string =
    let sz = boardSize |> string
    "<svg width=\"" + sz + "\" height=\"" + sz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\">\n" +
    "  <image href=\"Rink.png\" height=\"" + sz + "\" width=\"" + sz + "\" />\n" +
    (evalBoard board)
    + "</svg>\n"
