module Tom where
import GameCore
import Graphics.Gloss

-- Tom's movement --

turnLeft :: Direction -> Direction
turnLeft d =
    case d of
        North -> West
        South -> East
        West -> South
        East -> North

turnRight :: Direction -> Direction
turnRight d =
    case d of
        North -> East
        South -> West
        West -> North
        East -> South

turn :: Direction -> Direction
turn d =
    case d of
        North -> South
        South -> North
        West -> East
        East -> West

isBlocked :: Tom -> Grid -> Bool
isBlocked tom g = getSeg g (newPos (pos tom) (dir tom) 20) == 'W'

newPos :: Point -> Direction -> Float -> Point
newPos (x,y) d step =
    case d of
        North -> (x, y + step)
        South -> (x, y - step)
        East -> (x + step, y)
        West -> (x - step, y)

readCommand :: Tom -> Tom
readCommand tom@(Tom {commandQueue = []}) = tom
readCommand tom@(Tom {commandQueue = (command:commands)}) =
    case command of
        "walk" -> tom {
            endPoint = newPos (pos tom) (dir tom) 64,
            commandQueue = commands}
        "turnAround" -> tom {
            dir = turn (dir tom),
            commandQueue = commands
        }
        "turnLeft" -> tom {
            dir = turnLeft (dir tom),
            commandQueue = commands
        }
        "turnRight" -> tom {
            dir = turnRight (dir tom),
            commandQueue = commands
        }
    
updateTom :: Tom -> Grid -> Tom
updateTom tom g = 
    if endPoint tom == pos tom
        then readCommand (tom {walkFrame = 1})
        else move tom g

move :: Tom -> Grid -> Tom
move tom g =
    if isBlocked tom g
        then tom {
            walkFrame = 1, endPoint = pos tom}
            --, commandQueue = emptyCmdQ (commandQueue tom) "walk"}
        else tom {
            pos = newPos (pos tom) (dir tom) 4, 
            walkFrame = walkFrame tom + 1}

emptyCmdQ :: [String] -> String -> [String]
emptyCmdQ cmdQ s = filter (/= s) cmdQ