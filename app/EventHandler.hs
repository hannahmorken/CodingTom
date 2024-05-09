module EventHandler where
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import qualified Control.Monad.State as S

import GameCore
import Interpreter
import Text.Megaparsec
import System.Exit (exitSuccess)

eventHandler :: Event -> World -> IO World
eventHandler event world = do
    grid' <- loadGrid (level world)
    case gameState world of
        Menu    -> handleMenu event world grid'
        Game    -> handleGame event world
        TalkBox -> handleTalkBox event world

handleQuit :: Point -> World -> IO World
handleQuit mousePos world =
    if pointInBox mousePos (getUpperCorner quitButton) (getLowerCorner quitButton)
        then exitSuccess
        else return world

handleMenu :: Event -> World -> Grid -> IO World
handleMenu (EventMotion mousePos) world _ = checkHover mousePos world [playGameButton]
handleMenu (EventKey (MouseButton LeftButton) Down _ mousePos) world g = do
    _ <- handleQuit mousePos world
    if pointInBox mousePos (getUpperCorner playGameButton) (getLowerCorner playGameButton)
        then return $ initWorldGame 1 g
        else return world
handleMenu _ world _ = return world

handleTalkBox :: Event -> World -> IO World
handleTalkBox (EventMotion mousePos) world = checkHover mousePos world [okButton]
handleTalkBox (EventKey (MouseButton LeftButton) Down _ mousePos) world = do
    _ <- handleQuit mousePos world
    if pointInBox mousePos (getUpperCorner okButton) (getLowerCorner okButton)
        then return $ world {gameState = Game, hover = Nothing}
        else return world
handleTalkBox _ world = return world


handleGame :: Event -> World -> IO World
handleGame (EventKey (SpecialKey KeySpace) Down _ _) world =
    if tooLong (last' (code world))
        then return $ world {code = newLine (code world)}
        else return $ typeChar world ' '

handleGame (EventKey (Char c) Down _ _) world =
    if tooLong (last' (code world))
        then return $ world {code = nextLine (code world) (getLastWord (code world)) c}
        else return $ typeChar world c

handleGame (EventKey (SpecialKey KeyLeft) Down _ _) world = return $ world {code = backSpace (code world)}
handleGame (EventKey (SpecialKey KeyEnter) Down _ _) world = return $ world {code = newLine (code world)}

handleGame (EventMotion mousePos) world = checkHover mousePos world gameButtons

handleGame (EventKey (MouseButton LeftButton) Down _ mousePos) world = do
    _ <- handleQuit mousePos world

    if pointInBox mousePos (getUpperCorner typeButton) (getLowerCorner typeButton)
        then do
            case parse statements "" (unwords (code world)) of
                Left _ -> return world {codeColor = red}
                Right stmts -> return $ S.execState (evalMult stmts) (world {codeColor = black})

    else if pointInBox mousePos (getUpperCorner emptyButton) (getLowerCorner emptyButton)
        then return $ world {code = [], codeColor = black}

    else return world

handleGame _ world = return world

go :: World -> String
go world = if isAtGoal (tom world) (grid world) then "true" else "false"

typeChar :: World -> Char -> World
typeChar world c = world {code = init' (code world) ++ [last' (code world) ++ [c]]}

checkHover :: (Float, Float) -> World -> [Button] -> IO World
checkHover p world [] =
    if pointInBox p (getUpperCorner quitButton) (getLowerCorner quitButton)
        then return $ world {hover = Just quitButton}
        else return $ world {hover = Nothing}
checkHover p world (button:buttons) = do
    if pointInBox p (getUpperCorner button) (getLowerCorner button)
        then return $ world {hover = Just button}
        else checkHover p world buttons