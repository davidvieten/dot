module Parser
open AST
open Combinator

(*
    <expr> ::= <route>+
    <route>::= <start><endroute>
   <start> ::= <position><dotplace>
<dotplace> ::= <side><zone>
<position> ::= <player><location>
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
  <player> ::= leftwing
            |  rightwing
            |  center
            |  rightdefense
            |  leftdefense
<location> ::= lefthash
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
let player = 
    (pstr "leftwing" |>> (fun _ -> LeftWing)) <|>
    (pstr "rightwing" |>> (fun _ -> RightWing)) <|>
    (pstr "center" |>> (fun _ -> Center)) <|>
    (pstr "leftdefense" |>> (fun _ -> LeftDefense)) <|>
    (pstr "rightdefense" |>> (fun _ -> RightDefense))

let location (dotPlace: DotPlace) =
    match (dotPlace) with
    | (Left, Offense) -> 
            (pstr "lefthash" |>> (fun _ -> LeftHash { x = 170; y = 250 })) <|>
            (pstr "righthash" |>> (fun _ -> RightHash { x = 50; y = 250 })) <|>
            (pstr "dot" |>> (fun _ -> Dot { x = 107; y = 250 })) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint { x = 50; y = 155 })) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint { x = 170; y = 155 })) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside { x = 150; y = 220 })) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside { x = 65; y = 220 }))
                
    | (Left, Defense) -> 
            (pstr "lefthash" |>> (fun _ -> LeftHash { x = 0; y = 0 })) <|>
            (pstr "righthash" |>> (fun _ -> RightHash { x = 0; y = 0 })) <|>
            (pstr "dot" |>> (fun _ -> Dot { x = 0; y = 0 })) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint { x = 0; y = 0 })) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint { x = 0; y = 0 })) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside { x = 0; y = 0 })) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside { x = 0; y = 0 }))
                
    | (Right, Offense) -> 
            (pstr "lefthash" |>> (fun _ -> LeftHash { x = 0; y = 0 })) <|>
            (pstr "righthash" |>> (fun _ -> RightHash { x = 0; y = 0 })) <|>
            (pstr "dot" |>> (fun _ -> Dot { x = 0; y = 0 })) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint { x = 0; y = 0 })) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint { x = 0; y = 0 })) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside { x = 0; y = 0 })) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside { x = 0; y = 0 }))
                
    | (Right, Defense) -> 
            (pstr "lefthash" |>> (fun _ -> LeftHash { x = 0; y = 0 })) <|>
            (pstr "righthash" |>> (fun _ -> RightHash { x = 0; y = 0 })) <|>
            (pstr "dot" |>> (fun _ -> Dot { x = 0; y = 0 })) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint { x = 0; y = 0 })) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint { x = 0; y = 0 })) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside { x = 0; y = 0 })) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside { x = 0; y = 0 }))

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
        (fun sz -> (sz))
        
let position (dotPlace: DotPlace) =
    pseq
        (pad player)
        (pad (location dotPlace))
        (fun (p, l) -> (p, l)) |>> Position

let start (dotPlace: DotPlace)=
    pseq
        (pad (position dotPlace))
        (pad dotplace)
        (fun (p, d) ->
            (p, d)) |>> Start

//fine, just need to enter coordinates
let endroute = 
    (pstr "net" |>> (fun _ -> Net { x = 0; y = 0 })) <|>
    (pstr "walkline" |>> (fun _ -> WalkLine { x = 0; y = 0 })) <|>
    (pstr "upwall" |>> (fun _ -> UpWall { x = 0; y = 0 })) <|>
    (pstr "corner" |>> (fun _ -> Corner { x = 0; y = 0 })) <|>
    (pstr "hold" |>> (fun _ -> Hold { x = 0; y = 0 })) <|>
    (pstr "slot" |>> (fun _ -> Slot { x = 0; y = 0 })) <|>
    (pstr "backdoor" |>> (fun _ -> BackDoor { x = 0; y = 0 }))

let route (dotPlace: DotPlace) =
    pseq
        (pad (start dotPlace))  
        (pad endroute)
        (fun (s, e) -> { start = s; endRoute = e }) |>> (fun l -> [l])

exprImpl :=
    pmany1 (
        route (Left, Offense) <|>
        route (Left, Defense) <|>
        route (Right, Offense) <|>
        route (Right, Defense)
    ) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : Board option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None