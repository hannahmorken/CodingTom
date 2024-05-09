module Testing where
import Test.QuickCheck
import Data.List (intersperse)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Text.Megaparsec
import Text.Megaparsec.Char
import GameCore
import Tom
import EventHandler
import Interpreter
import Control.Monad (replicateM)
import Control.Monad.State

-- Tom's tests --

instance Arbitrary Direction where
    arbitrary = elements [North, South, West, East]

instance Arbitrary Tom where
    arbitrary = do
        point <- arbitrary
        direction <- arbitrary
        return Tom { pos = point, dir = direction, img = "", walkFrame = 0, endPoint = (0,0), commandQueue = [] }

instance Arbitrary Grid where
    arbitrary = do
        level <- elements [1..10]
        char <- elements "WE"
        cells <- sequence [generateCell x y | x <- [0..11], y <- [0..11]] :: Gen [(Float, Float, Char)]
        return $ Grid level cells

generateCell :: Float -> Float -> Gen (Float, Float, Char)
generateCell x y = do
    char <- elements "WE"  -- Randomly choose 'W' or 'E' for the cell type
    return (x, y, char)

-- Test that turning left twice is the same as turning around
prop_turnAround_turnLeftTwice :: Tom -> Property
prop_turnAround_turnLeftTwice tom =
    let
        turnATom = tom {dir = turn (dir tom)}
        turnLTom = tom {dir = turnLeft (turnLeft (dir tom))}
    in collect (dir tom) $ turnATom == turnLTom

-- Test that turning left four times is the same as not moving
prop_turnLeftFourTimes :: Tom -> Property
prop_turnLeftFourTimes tom =
    let turn = turnLeft $ turnLeft $ turnLeft $ turnLeft $ dir tom
    in collect (dir tom) $ tom == tom {dir = turn}

-- Test that walking when blocked is the same as not moving
prop_walk_when_blocked :: Tom -> Grid -> Property
prop_walk_when_blocked tom grid =
    isBlocked tom grid ==> pos (move tom grid) == pos tom


-- Interpreter tests --

generateValidString :: Gen String
generateValidString = do
    n <- choose (1, 20)
    strings <- replicateM n $ elements ["walk", "turnAround", "turnLeft", "turnRight"] 
    return $ unwords strings

-- Test that interpreting a valid input of commands gives the same resutl as interpreting each command individually
prop_interpret_cmds :: Property
prop_interpret_cmds =
    forAll generateValidString $ \input -> let
        cmds = parseCmds input
        cmdsIndv = mapM parseCmd (words input)
    in collect (lengthOfMaybeList cmds) $ cmds == cmdsIndv

lengthOfMaybeList :: Maybe [a] -> Maybe Int
lengthOfMaybeList Nothing = Nothing
lengthOfMaybeList (Just xs) = Just (length xs)

parseCmds :: String -> Maybe [Command]
parseCmds input =
    case parse commands "" input of
        Left _ -> Nothing
        Right cmds -> Just cmds

parseCmd :: String -> Maybe Command
parseCmd input =
    case parse command "" input of
        Left _ -> Nothing
        Right cmd -> Just cmd


-- given a while loop, check that it interprets it as a while loop, even though it has a long code block

-- if ------ "" --------

-- given a tom and valid input, check that the command list is correct
prop_check_commandQueue :: Grid -> Property
prop_check_commandQueue grid = 
    forAll generateValidString $ \input -> let
        stmt = parseStmt input
    in case stmt of
        Nothing -> discard
        Just stmt -> let
            world = execState (eval' stmt) (initWorldGame 1 grid)
            in commandQueue (tom world) == words input

parseStmt :: String -> Maybe Statement
parseStmt input = 
    case parse statement "" input of
        Left _ -> Nothing
        Right stmt -> Just stmt

-- Text and typing tests --

--write the code of input
--foldl (\c world -> handleGame (EventKey (Char c) Down _ _) world) initworld input

-- Test that a code of length n should be split into m lines



