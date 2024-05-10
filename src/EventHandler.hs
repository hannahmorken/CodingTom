module EventHandler where
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import qualified Control.Monad.State as S
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Megaparsec
import System.Exit (exitSuccess)

import GameCore
import Interpreter


eventHandler :: TMVar String -> TMVar TomState -> Event -> World -> IO World
eventHandler cVar sVar event world = do
    g <- loadGrid (level world)
    case gameState world of
        Menu    -> handleMenu cVar sVar event world g
        Game    -> handleGame event world
        TalkBox -> handleTalkBox event world


handleQuit :: Point -> World -> IO World
handleQuit mousePos world =
    if pointInBox mousePos (getUpperCorner quitButton) (getLowerCorner quitButton)
        then exitSuccess
        else return world


handleMenu :: TMVar String -> TMVar TomState -> Event -> World -> Grid -> IO World
handleMenu _ _ (EventMotion mousePos) world _ = checkHover mousePos world [playGameButton]
handleMenu cVar sVar (EventKey (MouseButton LeftButton) Down _ mousePos) world g = do
    _ <- handleQuit mousePos world
    if pointInBox mousePos (getUpperCorner playGameButton) (getLowerCorner playGameButton)
        then return $ initWorldGame cVar sVar 1 g
        else return world
handleMenu _ _ _ world _ = return world


handleTalkBox :: Event -> World -> IO World
handleTalkBox (EventMotion mousePos) world = checkHover mousePos world [okButton]
handleTalkBox (EventKey (MouseButton LeftButton) Down _ mousePos) world = do
    _ <- handleQuit mousePos world
    if pointInBox mousePos (getUpperCorner okButton) (getLowerCorner okButton)
        then if level world <= 4
            then return $ world {gameState = Game, hover = Nothing}
            else exitSuccess
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
                Right stmts -> do
                    threadID <- forkIO $ void $ S.execStateT (evalMult stmts) (world {codeColor = black})
                    return $ world {codeColor = black, interpThread = Just threadID}

    else if pointInBox mousePos (getUpperCorner emptyButton) (getLowerCorner emptyButton)
        then return $ world {code = [], codeColor = black}

    else if pointInBox mousePos (getUpperCorner hintButton) (getLowerCorner hintButton)
        then return $ world {gameState = TalkBox}

    else return world

handleGame _ world = return world


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