module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GameCore
import EventHandler
import Tom
import ImageLoader

import Testing
import Test.QuickCheck

import Interpreter
import Text.Megaparsec

window :: Display
window = InWindow "Coding Tom" windowSize (10, 10)

getCorrectImages :: Tom -> IO [Picture]
getCorrectImages tom =
    case dir tom of
        East  -> loadRightImages
        West  -> loadLeftImages
        North -> loadRightImages
        South -> loadRightImages

renderMenu :: World -> IO Picture
renderMenu world = do
    image <- loadBMP (img (tom world))
    return $ pictures [
        drawButton 0.8 playGameButton,
        drawButton 0.8 quitButton,
        hovering (hover world),
        uncurry translate (pos (tom world)) image]

renderTalkBox :: World -> (Int -> [String]) -> IO Picture
renderTalkBox world getText = do
    tomImg <- loadBMP tomStraight
    unitImg <- loadBMP wall
    game <- renderGame world
    return $ pictures [
        game,
        color (withAlpha 0.9 (greyN 0.5)) (rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)),
        color white (polygon talkBox),
        drawButton 0.8 okButton,
        drawButton 0.8 quitButton,
        hovering (hover world),
        --scale 2.2 2.2
        translate (-xCorner + 80) (-yCorner + 70) tomImg,
        drawText (getText (level world)) (-xCorner + 85, yCorner - 100) black,
        if level world == 1 
            then translate (-xCorner + 160) (-yCorner + 230) unitImg
            else blank]

printDir :: Direction -> String
printDir dir = case dir of
    North -> "nor"
    South -> "sou"
    West -> "west"
    East -> "east"

printBool :: Bool -> String
printBool True = "true"
printBool False = "false"

renderGame :: World -> IO Picture
renderGame world = do
    images <- getCorrectImages (tom world)
    file <- loadGrid (level world)
    gameMap <- drawGrid file
    putStrLn $ "is not at goal?: " ++ printBool (not $ isAtGoal (tom world) (grid world))
    putStrLn $ "cmdQ length: " ++ printBool (length (commandQueue (tom world)) < 20)
    putStrLn $ "is blocked?: " ++ printBool (isBlocked (tom world) (grid world))
    putStrLn $ "length of cmdQ: " ++ show (length (commandQueue (tom world)))
    putStrLn $ "commandQ: " ++ show (commandQueue (tom world))
    putStrLn ""
    --putStrLn $ "commands: " ++ unwords (commandQueue (tom world))
    --putStrLn $ "code: " ++ unwords (code world)
    --putStrLn $ "tom direction" ++ printDir (dir (tom world))
    return $ pictures [
        gameMap,
        drawButton 0.8 typeButton,
        drawButton 0.8 emptyButton,
        drawButton 0.8 quitButton,
        hovering (hover world),
        line [(lineX,-lineY),(lineX,lineY)],
        drawTom images world,
        drawText (code world) (-xCorner + (cellSize/2), yCorner - cellSize) (codeColor world)]

updateWorld :: Float -> World -> IO World
updateWorld _ world = do
    file <- loadGrid (level world)
    case gameState world of
        Menu -> return world
        Game -> return $ levelComplete world {tom = updateTom (tom world) file, grid = file}
        TalkBox -> return world

render :: World -> IO Picture
render world =
    case gameState world of
        Game    -> renderGame world
        Menu    -> renderMenu world
        TalkBox -> renderTalkBox world getWelcomeLevelText

main :: IO ()
main = playIO window white 7 initWorld render eventHandler updateWorld
    {-do 
        quickCheck prop_walk_when_blocked
        quickCheck prop_turnLeftFourTimes
        quickCheck prop_turnAround_turnLeftTwice
        quickCheck prop_interpret_cmds
        quickCheck prop_check_commandQueue-}
    {-do
        putStrLn "Enter a command: "
        input <- getLine
        case parse statements "" input of
            Left bundle -> putStrLn (errorBundlePretty bundle)
            Right cmd -> putStrLn $ "Parsed command: " ++ show cmd-}