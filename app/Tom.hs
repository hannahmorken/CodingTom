module Tom where
import GameCore
import Graphics.Gloss
import Control.Concurrent.STM
import Control.Monad

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
isBlocked t g = getSeg g (newPos (pos t) (dir t) 20) == 'W'

newPos :: Point -> Direction -> Float -> Point
newPos (x,y) d step =
    case d of
        North -> (x, y + step)
        South -> (x, y - step)
        East -> (x + step, y)
        West -> (x - step, y)

readCommand :: Tom -> IO Tom
readCommand t = do
    command <- atomically $ tryTakeTMVar (commandVar t)
    case command of
        Just "walk" -> return t {
            nextPoint = newPos (pos t) (dir t) 64}
        Just "turnAround" -> return t {
            dir = turn (dir t)}
        Just "turnLeft" -> return t {
            dir = turnLeft (dir t)}
        Just "turnRight" -> return t {
            dir = turnRight (dir t)}
        _ -> return t

forceTMVar :: TMVar TomState -> TomState -> STM ()
forceTMVar sVar s = do
    newVar <- tryPutTMVar sVar s
    if newVar 
        then return () 
        else void $ swapTMVar sVar s

updateTom :: Tom -> Grid -> IO Tom
updateTom t g =
    if nextPoint t == pos t
        then do
            atomically $ forceTMVar (stateVar t) Waiting
            readCommand (t {walkFrame = 1})
        else move t g

move :: Tom -> Grid -> IO Tom
move t g = do
    if isBlocked t g
        then do
            atomically $ forceTMVar (stateVar t) Stuck
            return t {
            walkFrame = 1, nextPoint = pos t}
        else do
            atomically $ forceTMVar (stateVar t) Moving
            return t {
            pos = newPos (pos t) (dir t) 4, 
            walkFrame = walkFrame t + 1}
