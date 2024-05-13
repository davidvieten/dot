module Parser
open AST
open Combinator

(*
    <expr> ::= <route>+
    <route>::= <routedef><dotplace>
<routedef> ::= <startroute><endroute>
<dotplace> ::= <side><zone>
<endroute> ::= net
            | walkline
            | downwall
            | upwall
            | corner
            | hold
            | slot
            | backdoor   
    <zone> ::= offense
            |  defense
    <side> ::= right
            |  left
<startroute> ::= lefthash
            |  righthash
            |  dot
            |  rightpoint
            |  leftpoint
            |  stackinside
            |  stackoutside   
*)
let pad p = pbetween pws0 p pws0
let expr, exprImpl = recparser()

(*parses a side and returns a primitive that will be used to make a dotplace*)
let side = 
    (pstr "right" |>> (fun _ -> Right)) <|>
    (pstr "left" |>> (fun _ -> Left))

(*parses a zone and returns a primitive that will be used to make a dotplace*)
let zone = 
    (pstr "offense" |>> (fun _ -> Offense)) <|>
    (pstr "defense" |>> (fun _ -> Defense)) 

(*parses a dotplace made up of a side and zone *)
let dotplace =
    pseq
        (pad side)
        (pad zone)
        (fun sz -> 
            (sz))|>>DotPlace

(*parses a startroute and returns a primitive*)
let startroute =
            (pstr "lefthash" |>> (fun _ -> LeftHash )) <|>
            (pstr "righthash" |>> (fun _ -> RightHash )) <|>
            (pstr "dot" |>> (fun _ -> Dot )) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint )) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint )) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside )) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside ))

(*parses a endroute and returns a primitive*)
let endroute =
            (pstr "net" |>> (fun _ -> Net )) <|>
            (pstr "walkline" |>> (fun _ -> WalkLine )) <|>
            (pstr "hold" |>> (fun _ -> Hold )) <|>
            (pstr "halfwall" |>> (fun _ -> HalfWall )) <|>
            (pstr "corner" |>> (fun _ -> Corner )) <|>
            (pstr "slot" |>> (fun _ -> Slot )) <|>
            (pstr "backdoor" |>> (fun _ -> BackDoor))

(*Parses a routedef, which is made up of a startroute and endroute*)
let routedef = 
        (pseq
            (pad (startroute))  
            (pad (endroute ))
            (fun (s, e) -> (s,e)))

(*Parses route/s and creates a list of routes*)
let route  =
    pseq
        (pad(routedef))
        (pad(dotplace))
        (fun (s, d) -> { routedef = s; dotplace = d}) |>> (fun l -> [l])

exprImpl :=
    pmany1 (
        route 
    ) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : Board option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None