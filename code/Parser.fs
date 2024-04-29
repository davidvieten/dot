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
    match dotPlace with
    | (side, zone) ->
        let x, y = 
            match side, zone with
            | Left, Offense -> 360, 250
            | Left, Defense -> 50, 282
            | Right, Offense -> 235, 250
            | Right, Defense -> 235, 282
        match x, y with
        | x, y -> 
            (pstr "lefthash" |>> (fun _ -> LeftHash { x = x; y = y })) <|>
            (pstr "righthash" |>> (fun _ -> RightHash { x = x; y = y })) <|>
            (pstr "dot" |>> (fun _ -> Dot { x = x; y = y })) <|>
            (pstr "rightpoint" |>> (fun _ -> RightPoint { x = x; y = y - 95 })) <|>
            (pstr "leftpoint" |>> (fun _ -> LeftPoint { x = x + 120; y = y - 95 })) <|>
            (pstr "stackinside" |>> (fun _ -> StackInside { x = x + 110; y = y - 30 })) <|>
            (pstr "stackoutside" |>> (fun _ -> StackOutside { x = x - 85; y = y - 30 }))

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
        
let position (dotPlace: DotPlace) =
    pseq
        (pad player)
        (pad (location dotPlace))
        (fun (p, l) -> 
            (p, l)) |>> Position

let start (dotPlace: DotPlace)=
    pseq
        (pad (position dotPlace))
        (pad dotplace)
        (fun (p, d) ->
            (p, d)) |>> Start

let endroute (dotPlace: DotPlace) =
    match (dotPlace) with
    | (Left, Offense) -> 
            (pstr "net" |>> (fun _ -> Net { x = 200; y = 300 })) <|>
            (pstr "walkline" |>> (fun _ -> WalkLine { x = 120; y = 170 })) <|>
            (pstr "upwall" |>> (fun _ -> UpWall { x = 360; y = 200 })) <|>
            (pstr "corner" |>> (fun _ -> Corner { x = 330; y = 360 })) <|>
            (pstr "slot" |>> (fun _ -> Slot { x = 200; y = 240 })) <|>
            (pstr "backdoor" |>> (fun _ -> BackDoor { x = 170; y = 310 }))
                
    | (Left, Defense) -> 
            (pstr "net" |>> (fun _ -> Net { x = 200; y = 300 })) <|>
            (pstr "walkline" |>> (fun _ -> WalkLine { x = 200; y = 370 })) <|>
            (pstr "upwall" |>> (fun _ -> UpWall { x = 40; y = 200 })) <|>
            (pstr "corner" |>> (fun _ -> Corner { x = 70; y = 360 })) <|>
            (pstr "slot" |>> (fun _ -> Slot { x = 200; y = 240 })) <|>
            (pstr "backdoor" |>> (fun _ -> BackDoor { x = 170; y = 310 }))
                
    | (Right, Offense) -> 
            (pstr "net" |>> (fun _ -> Net { x = 200; y = 300 })) <|>
            (pstr "walkline" |>> (fun _ -> WalkLine { x = 280; y = 170 })) <|>
            (pstr "upwall" |>> (fun _ -> UpWall { x = 40; y = 200 })) <|>
            (pstr "corner" |>> (fun _ -> Corner { x = 70; y = 360 })) <|>
            (pstr "slot" |>> (fun _ -> Slot { x = 200; y = 240 })) <|>
            (pstr "backdoor" |>> (fun _ -> BackDoor { x = 230; y = 310 }))
                
    | (Right, Defense) -> 
            (pstr "net" |>> (fun _ -> Net { x = 200; y = 300 })) <|>
            (pstr "walkline" |>> (fun _ -> WalkLine { x = 200; y = 370 })) <|>
            (pstr "upwall" |>> (fun _ -> UpWall { x = 360; y = 200 })) <|>
            (pstr "corner" |>> (fun _ -> Corner { x = 330; y = 360 })) <|>
            (pstr "slot" |>> (fun _ -> Slot { x = 200; y = 240 })) <|>
            (pstr "backdoor" |>> (fun _ -> BackDoor { x = 230; y = 310 }))

let route (dotPlace: DotPlace) =
    pseq
        (pad (start dotPlace))  
        (pad (endroute dotPlace))
        (fun (s, e) -> { start = s; endRoute = e }) |>> (fun l -> [l])

exprImpl :=
    pmany1 (
        route (Left, Defense) <|>
        route (Left, Offense) <|>
        route (Right, Offense) <|>
        route (Right, Defense)
    ) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : Board option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None