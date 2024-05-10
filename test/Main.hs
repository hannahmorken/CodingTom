module Main where
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Text.Megaparsec
import GameCore
import Tom
import Interpreter
import Control.Monad.State

instance Arbitrary Tom where
    arbitrary = do
        point <- arbitrary
        direction <- elements [North, South, West, East]
        return Tom { pos = point, dir = direction, walkFrame = 0, nextPoint = (0,0)}

instance Arbitrary Grid where
    arbitrary = do
        l <- elements [1..10]
        cells <- sequence [generateCell x y | x <- [0..11], y <- [0..11]] :: Gen [(Float, Float, Char)]
        return $ Grid l cells

generateCell :: Float -> Float -> Gen (Float, Float, Char)
generateCell x y = do
    char <- elements "WE"
    return (x, y, char)


-- Tom's tests --

-- Test that turning left twice is the same as turning around
prop_turnAround_turnLeftTwice :: Tom -> Property
prop_turnAround_turnLeftTwice t =
    let
        turnATom = t {dir = turn (dir t)}
        turnLTom = t {dir = turnLeft (turnLeft (dir t))}
    in collect (dir t) $ turnATom == turnLTom

-- Test that turning left four times is the same as not moving
prop_turnLeftFourTimes :: Tom -> Property
prop_turnLeftFourTimes t =
    let turn4 = turnLeft $ turnLeft $ turnLeft $ turnLeft $ dir t
    in collect (dir t) $ t == t {dir = turn4}

-- Test that walking when blocked is the same as not moving
prop_walk_when_blocked :: Tom -> Grid -> Property
prop_walk_when_blocked t g = monadicIO $ do
    t' <- run $ move t g
    pre $ isBlocked t g
    assert (pos t' == pos t)


-- Interpreter tests --

generateValidString :: Gen String
generateValidString = do
    n <- choose (1, 20)
    strings <- replicateM n $ elements ["walk", "turnAround", "turnLeft", "turnRight"] 
    return $ unwords strings

-- Test that interpreting a valid input of commands gives the same result 
-- as interpreting each command individually
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

parseStmt :: String -> Maybe Statement
parseStmt input = 
    case parse statement "" input of
        Left _ -> Nothing
        Right stmt -> Just stmt


main :: IO ()
main = do 
        quickCheck prop_walk_when_blocked
        quickCheck prop_turnLeftFourTimes
        quickCheck prop_turnAround_turnLeftTwice
        quickCheck prop_interpret_cmds