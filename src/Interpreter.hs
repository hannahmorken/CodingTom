module Interpreter where
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (when)
import qualified Control.Monad.State as S
import Control.Concurrent.STM
import Control.Monad.IO.Class

import GameCore

type Interpreter = S.StateT World IO

type Parser = Parsec Void String

data Command = Walk
    | TurnLeft
    | TurnRight
    | TurnAround
    deriving (Show, Eq)

data State' = Blocked
    | NotBlocked
    | Finished
    | NotFinished
    deriving (Show)

data Statement =
    While State' Statement |
    If State' Statement Statement | CmdList [Command]
    deriving (Show)

state :: Parser State'
state = Blocked <$ string "blocked"
    <|> NotBlocked <$ string "notBlocked"
    <|> Finished <$ string "finished"
    <|> NotFinished <$ string "notFinished"

command :: Parser Command
command = space *> choice [
    Walk <$ string "walk",
    TurnLeft <$ string "turnLeft",
    TurnRight <$ string "turnRight",
    TurnAround <$ string "turnAround"] <* space

commands :: Parser [Command]
commands = some command

ifStatement :: Parser Statement
ifStatement = If <$> (
    string "if" *> space *> state <* space) 
    <*> (char '(' *> statement <* char ')' <* space) 
    <*> (string "else" <* space *> char '(' *> statement <* char ')')

whileStatement :: Parser Statement
whileStatement = While <$> (
    string "while" *> space *> state) 
    <*> (space *> char '(' *> statement <* char ')')

statement :: Parser Statement
statement = space *> choice
    [whileStatement,
    ifStatement,
    CmdList <$> commands] <* space

statements :: Parser [Statement]
statements = some statement

eval' :: Statement -> Interpreter ()
eval' stmt = do
    sVar <- S.gets (stateVar . tom)
    b <- liftIO $ atomically $ do
        s <- takeTMVar sVar
        case s of
            Moving  -> retry
            Waiting -> return False
            Stuck   -> return True
    S.modify (\world -> do
        let t = tom world
        world {tom = t {blocked = b}})
    eval stmt


eval :: Statement -> Interpreter ()
eval (CmdList []) = return ()
eval (CmdList (c:cs)) = do
    case c of
        Walk -> evalCommand "walk"
        TurnAround -> evalCommand "turnAround"
        TurnLeft -> evalCommand "turnLeft"
        TurnRight -> evalCommand "turnRight"
    eval (CmdList cs)

eval (If s stmt eStmt) = do
    world <- S.get
    if readState s world 
        then eval stmt
        else eval eStmt

eval (While s stmt) = do
    world <- S.get
    when (readState s world) $ do
        eval' stmt
        eval (While s stmt)

evalCommand :: String -> Interpreter ()
evalCommand cmd = do
    world <- S.get
    let cVar = commandVar (tom world)
    liftIO $ atomically $ putTMVar cVar cmd
    return ()

evalMult :: [Statement] -> Interpreter ()
evalMult [] = return ()
evalMult (stmt:stmts) = do
    eval' stmt
    evalMult stmts

readState :: State' -> World -> Bool
readState Blocked world = blocked (tom world)
readState Finished world = isAtGoal (tom world) (grid world)
readState NotBlocked world = not $ blocked (tom world)
readState NotFinished world = not $ isAtGoal (tom world) (grid world)
