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

(*Parses the player string*)

let startroute =
            (pstr "lefthash" |>> (fun _ -> LeftHash )) <|>
            (pstr "righthash" |>> (fun _ -> RightHash )) <|>
            (pstr "dot" |>> (fun _ -> Dot )) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint )) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint )) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside )) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside ))
let side = 
    (pstr "right" |>> (fun _ -> Right)) <|>
    (pstr "left" |>> (fun _ -> Left))

let zone = 
    (pstr "offense" |>> (fun _ -> Offense)) <|>
    (pstr "defense" |>> (fun _ -> Defense)) 

let dotplace =
    pseq
        (pad side)
        (pad zone)
        (fun sz -> 
            (sz))|>>DotPlace


let endroute =
            (pstr "net" |>> (fun _ -> Net )) <|>
            (pstr "walkline" |>> (fun _ -> WalkLine )) <|>
            (pstr "hold" |>> (fun _ -> Hold )) <|>
            (pstr "halfwall" |>> (fun _ -> HalfWall )) <|>
            (pstr "corner" |>> (fun _ -> Corner )) <|>
            (pstr "slot" |>> (fun _ -> Slot )) <|>
            (pstr "backdoor" |>> (fun _ -> BackDoor))

let routedef = 
        (pseq
            (pad (startroute))  
            (pad (endroute ))
            (fun (s, e) -> (s,e)))

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