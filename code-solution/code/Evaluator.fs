module Evaluator

open AST

let rec evalDot (dotplace: DotPlace) = 
    match dotplace with
    |Right, Offense -> {x=296; y = 250} 
    |Left, Offense -> {x=107; y = 250}
    |Right, Defense -> {x=296; y = 280}
    |Left, Defense -> {x=107; y = 280}

let rec evalRouteStart (routedef:RouteDef) (dotplace: DotPlace)=
    let dot = evalDot (dotplace)
    match dotplace with 
    |Left, Offense ->
        match routedef with
        | LeftHash, _-> {x = dot.x - 57; y = dot.y}
        | RightHash, _ -> {x = dot.x + 57; y = dot.y}
        | LeftPoint, _ -> {x = dot.x - 57; y = dot.y - 90}
        | RightPoint, _ -> {x = dot.x + 57 ; y = dot.y - 90}
        | Dot, _ -> {x = dot.x; y = dot.y}
        | StackInside, _ -> {x = dot.x + 47; y = dot.y - 30}
        | StackOutside, _-> {x = dot.x - 47; y = dot.y - 30}
    |Right, Offense ->
        match routedef with
        | LeftHash, _-> {x = dot.x - 57; y = dot.y}
        | RightHash, _ -> {x = dot.x + 57; y = dot.y}
        | LeftPoint, _ -> {x = dot.x - 57; y = dot.y - 90}
        | RightPoint, _ -> {x = dot.x + 57 ; y = dot.y - 90}
        | Dot, _ -> {x = dot.x; y = dot.y}
        | StackInside, _ -> {x = dot.x - 47; y = dot.y - 30}
        | StackOutside, _-> {x = dot.x + 47; y = dot.y - 30}
    |Left, Defense ->
        match routedef with
        | LeftHash, _-> {x = dot.x - 57; y = dot.y}
        | RightHash, _ -> {x = dot.x + 57; y = dot.y}
        | LeftPoint, _ -> {x = dot.x - 57; y = dot.y + 30}
        | RightPoint, _ -> {x = dot.x + 57 ; y = dot.y + 30}
        | Dot, _ -> {x = dot.x; y = dot.y}
        | StackInside, _ -> {x = dot.x + 47; y = dot.y + 30}
        | StackOutside, _-> {x = dot.x; y = dot.y + 50}
    |Right, Defense ->
        match routedef with
        | LeftHash, _-> {x = dot.x - 57; y = dot.y}
        | RightHash, _ -> {x = dot.x + 57; y = dot.y}
        | LeftPoint, _ -> {x = dot.x - 57; y = dot.y + 30}
        | RightPoint, _ -> {x = dot.x + 57 ; y = dot.y + 30}
        | Dot, _ -> {x = dot.x; y = dot.y}
        | StackInside, _ -> {x = dot.x - 47; y = dot.y + 30}
        | StackOutside, _-> {x = dot.x; y = dot.y + 50}

let rec evalRouteEnd (routedef:RouteDef) (dotplace: DotPlace)=
    let dot = evalDot (dotplace)
    let start = evalRouteStart routedef dotplace
    match dotplace with 
    |Left, Offense ->
        match routedef with
        | _, Net -> {x = 200; y = 300}
        | _, WalkLine -> {x = start.x + 100 ; y = start.y}
        | _, Hold -> {x = start.x; y = start.y}
        | _, HalfWall -> {x = dot.x - 80; y = dot.y - 30}
        | _, Corner -> {x = dot.x - 67; y = dot.y + 100}
        | _, Slot -> {x = 200; y = 270}
        | _, BackDoor -> {x = dot.x + 113; y = dot.y + 60}
    |Right, Offense ->
        match routedef with
        | _, Net -> {x = 200; y = 300}
        | _, WalkLine -> {x = start.x - 100 ; y = start.y}
        | _, Hold -> {x = start.x; y = start.y}
        | _, HalfWall -> {x = dot.x + 80; y = dot.y - 30}
        | _, Corner -> {x = dot.x + 67; y = dot.y + 100}
        | _, Slot -> {x = 200; y = 270}
        | _, BackDoor -> {x = dot.x - 113; y = dot.y + 60}
    |Left, Defense ->
        match routedef with
        | _, Net -> {x = 200; y = 300}
        | _, WalkLine -> {x = dot.x + 93; y = dot.y + 100}
        | _, Hold -> {x = start.x; y = start.y}
        | _, HalfWall -> {x = dot.x - 87; y = dot.y - 50}
        | _, Corner -> {x = dot.x - 60; y = dot.y + 70}
        | _, Slot -> {x = 200; y = 270}
        | _, BackDoor -> {x = dot.x + 113; y = dot.y + 30}
    |Right, Defense ->
        match routedef with
        | _, Net -> {x = 200; y = 300}
        | _, WalkLine -> {x = dot.x - 93; y = dot.y + 100}
        | _, Hold -> {x = start.x; y = start.y}
        | _, HalfWall -> {x = dot.x + 87; y = dot.y - 50}
        | _, Corner -> {x = dot.x + 60; y = dot.y + 70}
        | _, Slot -> {x = 200; y = 270}
        | _, BackDoor -> {x = dot.x - 113; y = dot.y + 30}




let rec evalRoute (routedef: RouteDef) (dotplace: DotPlace): string =
    let start = evalRouteStart(routedef)(dotplace)
    let finish = evalRouteEnd (routedef)(dotplace)
    "  <circle cx=\"" + (start.x |> string) + "\"" +
    " cy=\"" + (start.y |> string) + "\"" +
    " r=\"5\" fill=\"black\" />\n" +
    "  <circle cx=\"" + (finish.x |> string) + "\"" +
    " cy=\"" + (finish.y |> string) + "\"" +
    " r=\"5\" fill=\"black\" />\n" +
    "  <line x1=\"" + (start.x |> string) + "\"" +
    " y1=\"" + (start.y |> string) + "\"" +
    " x2=\"" + (finish.x|> string) + "\"" +
    " y2=\"" + (finish.y|> string) + "\"" +
    " style=\"stroke:black;stroke-width:3\" />\n"


let rec evalBoard (board: Board) : string =
    match board with
    | [] -> ""
    | { routedef = routedef; dotplace = dotplace }::ls -> (evalRoute routedef dotplace) + (evalBoard ls)


let eval (board: Board) : string =
    let sz = boardSize |> string
    "<svg width=\"" + sz + "\" height=\"" + sz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\">\n" +
        "  <image href=\"Rink.png\" height=\"" + sz + "\" width=\"" + sz + "\" />\n" +
        (evalBoard board)
        + "</svg>\n"