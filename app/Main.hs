module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GameCore
import EventHandler
import Tom
import ImageLoader
import Control.Concurrent.STM

window :: Display
window = InWindow "Coding Tom" windowSize (10, 10)

getCorrectImages :: Tom -> IO [Picture]
getCorrectImages t =
    case dir t of
        East  -> loadRightImages
        West  -> loadLeftImages
        North -> loadRightImages
        South -> loadRightImages

renderMenu :: World -> IO Picture
renderMenu world = do
    image <- loadBMP tomStraight
    return $ pictures [
        drawButton 0.8 playGameButton,
        drawButton 0.8 quitButton,
        hovering (hover world),
        uncurry translate (pos (tom world)) $ scale 3 3 image]

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
        translate (-xCorner + 80) (-yCorner + 70) $ scale 2.2 2.2 tomImg,
        drawText (getText (level world)) (-xCorner + 85, yCorner - 100) black,
        if level world == 1 
            then translate (-xCorner + 160) (-yCorner + 230) unitImg
            else blank]

renderGame :: World -> IO Picture
renderGame world = do
    images <- getCorrectImages (tom world)
    file <- loadGrid (level world)
    gameMap <- drawGrid file
    return $ pictures [
        gameMap,
        drawButton 0.8 typeButton,
        drawButton 0.8 emptyButton,
        drawButton 0.8 quitButton,
        drawButton 0.8 hintButton,
        hovering (hover world),
        line [(lineX,-lineY),(lineX,lineY)],
        drawTom images world,
        drawText (code world) (-xCorner + (cellSize/2), yCorner - cellSize) (codeColor world)]

updateWorld :: TMVar String -> TMVar TomState -> Float -> World -> IO World
updateWorld cVar sVar _ world = do
    file <- loadGrid (level world)
    case gameState world of
        Menu -> return world
        Game -> do        
            newTom <- updateTom (tom world) file
            levelComplete cVar sVar world {tom = newTom, grid = file}
        TalkBox -> return world

render :: World -> IO Picture
render world =
    case gameState world of
        Game    -> renderGame world
        Menu    -> renderMenu world
        TalkBox -> renderTalkBox world getWelcomeLevelText

main :: IO ()
main = do
    initG <- loadGrid 0
    cVar <- newEmptyTMVarIO
    sVar <- newEmptyTMVarIO
    playIO window white 7 (initWorld cVar sVar initG) render (eventHandler cVar sVar) (updateWorld cVar sVar)