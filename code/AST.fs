module AST

type Coordinate = {x: int; y: int}
type Player = 
    | LeftWing
    | RightWing
    | Center
    | RightDefense
    | LeftDefense

type StartPos = 
    | LeftHash of Coordinate
    | RightHash of Coordinate
    | Dot of Coordinate
    | RightPoint of Coordinate
    | LeftPoint of Coordinate
    | StackInside of Coordinate
    | StackOutside of Coordinate

type EndRoute = 
    | Net of Coordinate
    | WalkLine of Coordinate
    | DownWall of Coordinate
    | UpWall of Coordinate
    | Corner of Coordinate
    | Hold of Coordinate
    | Slot of Coordinate
    | BackDoor of Coordinate

type Route = {start: StartPos; endRoute: EndRoute; player:Player}
type Board = Route list

   

let canvasSize = 400
