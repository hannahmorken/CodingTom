module Interpreter where
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (liftA2)
import Control.Monad (when)
import qualified Control.Monad.State as S

import Tom
import GameCore
import Data.List (group)

type Interpreter = S.State World

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
    If State' Statement | CmdList [Command]
    deriving (Show)

state' :: Parser State'
state' = Blocked <$ string "blocked"
    <|> NotBlocked <$ string "not blocked"
    <|> Finished <$ string "finished"
    <|> NotFinished <$ string "not finished"

command :: Parser Command
command = space *> choice [
    Walk <$ string "walk",
    TurnLeft <$ string "turn left",
    TurnRight <$ string "turn right",
    TurnAround <$ string "turn around"] <* space

commands :: Parser [Command]
commands = some command

statement :: Parser Statement
statement = space *> choice
    [While <$> (string "while" *> space *> state' <* char ',') <*> (space *> statement),
    If <$> (string "if" *> space *> state' <* char ',') <*> (space *> string "then" *> space *> statement),
    CmdList <$> commands] <* space

statements :: Parser [Statement]
statements = some statement

-- TODO: Make this prettier
eval' :: Statement -> Interpreter ()
eval' (CmdList []) = return ()
eval' (CmdList (command:commands)) = do
    case command of
        Walk -> evalCommand "walk"
        TurnAround -> evalCommand "turn"
        TurnLeft -> evalCommand "turnL"
    eval' (CmdList commands)

eval' (If state statement) = do
    world <- S.get
    when (readState state world) $ eval' statement

eval' (While state statement) = do
    world <- S.get
    let cmdQ = commandQueue (tom world)
    when (readState state world && (length cmdQ < 20)) $ do
        eval' statement
        eval' (While state statement)

evalCommand :: String -> Interpreter ()
evalCommand cmd = do
    world <- S.get
    let tom' = tom world
    let newTom = tom' {commandQueue = commandQueue tom' ++ [cmd]}
    S.put world {tom = newTom}

evalMult' :: [Statement] -> Interpreter ()
evalMult' [] = return ()
evalMult' (stmt:stmts) = do
    eval' stmt
    evalMult' stmts

readState :: State' -> World -> Bool
readState Blocked world = isBlocked (tom world) (grid world)
readState Finished world = isAtGoal (tom world) (grid world)
readState NotBlocked world = not $ isBlocked (tom world) (grid world)
readState NotFinished world = not $ isAtGoal (tom world) (grid world)

evalMult :: [Statement] -> Tom -> Grid -> Tom
evalMult stmts tom grid
  = foldl (\tom stmt -> eval stmt tom grid) tom stmts

eval :: Statement -> Tom -> Grid -> Tom
eval (CmdList commands) tom _ =
    tom {commandQueue = makeStringCommands commands} where
        makeStringCommands [] = []
        makeStringCommands (cmd:cmds) =
            case cmd of
                Walk -> "walk" : makeStringCommands cmds
                TurnAround -> "turn" : makeStringCommands cmds

{-eval (If state statement) tom grid =
    if readState state tom grid
        then eval statement tom grid
        else tom-}

-- This doesnt work --
eval (While state statement) tom grid = undefined
