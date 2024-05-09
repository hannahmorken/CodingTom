module Interpreter where
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (when)
import qualified Control.Monad.State as S

import Tom
import GameCore

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
    <*> (char '(' *> statement <* space) 
    <*> (string "else" *> statement <* char ')')

statement :: Parser Statement
statement = space *> choice
    [While <$> (string "while" *> space *> state) <*> (space *> char '(' *> statement <* char ')'),
    ifStatement,
    --If <$> (string "if" *> space *> state' <* string " then") <*> (space *> char '(' *> statement <* char ')'),
    CmdList <$> commands] <* space

statements :: Parser [Statement]
statements = some statement

-- TODO: Make this prettier
eval :: Statement -> Interpreter ()
eval (CmdList []) = return ()
eval (CmdList (c:cs)) = do
    case c of
        Walk -> evalCommand "walk"
        TurnAround -> evalCommand "turnAround"
        TurnLeft -> evalCommand "turnLeft"
        TurnRight -> evalCommand "turnRight"
    eval (CmdList cs)

eval (If state stmt elseStmt) = do
    world <- S.get
    if readState state world 
        then eval stmt
        else eval elseStmt

eval (While state stmt) = do
    world <- S.get
    let cmdQ = commandQueue (tom world)
    when (readState state world && (length cmdQ < 20)) $ do
        eval stmt
        eval (While state stmt)

evalCommand :: String -> Interpreter ()
evalCommand cmd = do
    world <- S.get
    let t = tom world
    let newTom = t {commandQueue = commandQueue t ++ [cmd]}
    S.put world {tom = newTom}

evalMult :: [Statement] -> Interpreter ()
evalMult [] = return ()
evalMult (stmt:stmts) = do
    eval stmt
    evalMult stmts

readState :: State' -> World -> Bool
readState Blocked world = isBlocked (tom world) (grid world)
readState Finished world = isAtGoal (tom world) (grid world)
readState NotBlocked world = not $ isBlocked (tom world) (grid world)
readState NotFinished world = not $ isAtGoal (tom world) (grid world)