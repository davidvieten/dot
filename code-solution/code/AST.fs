module AST

type Coordinate = { x: int; y: int }

type Side =
    | Left
    | Right

type Zone =
    | Offense
    | Defense

type DotPlace = Side * Zone

type StartRoute =
    | LeftHash 
    | RightHash 
    | Dot 
    | RightPoint 
    | LeftPoint 
    | StackInside 
    | StackOutside 

type EndRoute =
    | Net 
    | WalkLine 
    | Hold 
    | HalfWall 
    | Corner 
    | Slot 
    | BackDoor 

type RouteDef = StartRoute * EndRoute

type Route =
    { routedef: RouteDef; dotplace: DotPlace}

type Board = Route list


let boardSize = 400