module Tom where
import GameCore
import ImageLoader
import Graphics.Gloss

-- Tom's movement --

turnLeft :: Direction -> Direction
turnLeft dir =
    case dir of
        North -> West
        South -> East
        West -> South
        East -> North

turn :: Direction -> Direction
turn dir =
    case dir of
        North -> West
        South -> East
        West -> East
        East -> West

isBlocked :: Tom -> Grid -> Bool
isBlocked tom grid = getSeg grid (newPos (pos tom) (dir tom) 20) == 'W'

newPos :: Point -> Direction -> Float -> Point
newPos (x,y) dir step =
    case dir of
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
        "turn" -> tom {
            dir = turn (dir tom),
            commandQueue = commands
        }
        "turnL" -> tom {
            dir = turnLeft (dir tom),
            commandQueue = commands
        }
    
updateTom :: Tom -> Grid -> Tom
updateTom tom grid = 
    if endPoint tom == pos tom
        then readCommand (tom {walkFrame = 1})
        else move tom grid

move :: Tom -> Grid -> Tom
move tom grid =
    if isBlocked tom grid
        then tom {walkFrame = 1, endPoint = pos tom}
        else tom {
            pos = newPos (pos tom) (dir tom) 4, 
            walkFrame = walkFrame tom + 1}