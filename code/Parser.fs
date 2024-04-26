module Parser
open AST
open Combinator


(*
Need to fix grammar to make sense:
    <expr> ::= <location><play>
<location> ::= <zone><side>
    <zone> ::= offense
            |  defense
    <side> ::= right
            |  left
    <play> ::= <routedef>+
            |  <empty>
<routedef> ::= <player><route>
   <route> ::= <startpos><endroute>
  <player> ::= leftwing
            |  rightwing
            |  center
            |  rightdefense
            |  leftdefense
<startpos> ::= lefthash
            |  righthash
            |  dot
            |  rightpoint
            |  leftpoint
            |  stackinside
            |  stackoutside
<endroute> ::= net
            | walkline
            | downwall
            | upwall
            | corner
            | hold
            | slot
            | backdoor       
*)
let pad p = pbetween pws0 p pws0
let expr, exprImpl = recparser()

let drawRoute (startPos, endRoute, player): Route =
    { start = startPos; endRoute = endRoute; player = player }

(*Parses the player string*)
let player = 
    (pstr "leftwing" |>> (fun _ -> LeftWing)) <|>
    (pstr "rightwing" |>> (fun _ -> RightWing)) <|>
    (pstr "center" |>> (fun _ -> Center)) <|>
    (pstr "leftdefense" |>> (fun _ -> LeftDefense)) <|>
    (pstr "rightdefense" |>> (fun _ -> RightDefense))

let startpos = 
    (pstr "lefthash" |>> (fun _ -> LeftHash { x = 0; y = 0 })) <|>
    (pstr "righthash" |>> (fun _ -> RightHash { x = 0; y = 0 })) <|>
    (pstr "dot" |>> (fun _ -> Dot { x = 0; y = 0 })) <|>
    (pstr "rightpoint" |>> (fun _ -> RightPoint { x = 0; y = 0 })) <|>
    (pstr "leftpoint" |>> (fun _ -> LeftPoint { x = 0; y = 0 })) <|>
    (pstr "stackinside" |>> (fun _ -> StackInside { x = 0; y = 0 })) <|>
    (pstr "stackoutside" |>> (fun _ -> StackOutside { x = 0; y = 0 }))

let endroute = 
    (pstr "net" |>> (fun _ -> Net { x = 0; y = 0 })) <|>
    (pstr "walkline" |>> (fun _ -> WalkLine { x = 0; y = 0 })) <|>
    (pstr "upwall" |>> (fun _ -> UpWall { x = 0; y = 0 })) <|>
    (pstr "corner" |>> (fun _ -> Corner { x = 0; y = 0 })) <|>
    (pstr "hold" |>> (fun _ -> Hold { x = 0; y = 0 })) <|>
    (pstr "slot" |>> (fun _ -> Slot { x = 0; y = 0 })) <|>
    (pstr "backdoor" |>> (fun _ -> BackDoor { x = 0; y = 0 }))
let route = 
    pseq
        (pad (startpos))
        (pad (endroute))
        (fun (s, e) -> (s, e))

let routedef = 
    pseq
        (pad (player))
        (pad (route))
        (fun (p, (s, e)) -> drawRoute (s, e, p))

exprImpl :=
    pmany1 (
        routedef |>> (fun l -> [l])
    ) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : Board option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None