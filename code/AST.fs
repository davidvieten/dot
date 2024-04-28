module AST

type Coordinate = { x: int; y: int }

type Side =
    | Left
    | Right

type Zone =
    | Offense
    | Defense

type DotPlace = Side * Zone

type Player =
    | LeftWing
    | RightWing
    | Center
    | RightDefense
    | LeftDefense

type Location =
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

type Position = Player * Location
type Start = Position * DotPlace

type Route =
    { start: Start; endRoute: EndRoute }

type Board = Route list


let canvasSize = 400