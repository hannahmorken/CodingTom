{-# LANGUAGE RecordWildCards #-}

module GameCore where
import Graphics.Gloss
import ImageLoader
import Control.Concurrent
import Control.Concurrent.STM
import Data.List (find)


-- Core data types in the game --

data Tom = Tom {
    pos :: Point,
    dir :: Direction,
    walkFrame :: Int,
    nextPoint :: Point,
    commandVar :: TMVar String,
    stateVar :: TMVar TomState,
    blocked :: Bool
} deriving (Eq)

data TomState = Moving | Waiting | Stuck

instance Show Tom where
    show Tom{..} = concat
        [ "Tom {pos =", show pos, ", dir = ", show dir
        , ", walkFrame = ", show walkFrame, ", nextPoint = "
        , show nextPoint, "}"]

data World = World {
    tom :: Tom,
    code :: [String],
    gameState :: GameState,
    level :: Int,
    grid :: Grid,
    hover :: Maybe Button,
    codeColor :: Color,
    interpThread :: Maybe ThreadId
}

data Direction = North | South | West | East deriving (Eq, Show)

data GameState = Menu | Game | TalkBox

data Grid = Grid Int [(Float, Float, Char)] deriving (Show)

data Button = Button {
    middle :: Point,
    height :: Float,
    width :: Float,
    txt :: String,
    txtPos :: Point
}


-- Initial menu and game world --

initWorld :: TMVar String -> TMVar TomState -> Grid -> World
initWorld cVar sVar g = World {
    tom = Tom {pos = (0,100), dir = East, walkFrame = 1, nextPoint = (0,100), commandVar = cVar, stateVar = sVar, blocked = False},
    code = [],
    gameState = Menu,
    level = 0,
    grid = g,
    hover = Nothing,
    codeColor = black,
    interpThread = Nothing
}

startTom :: TMVar String -> TMVar TomState -> Tom
startTom cVar sVar = Tom {
    pos = (0,32),
    dir = East,
    walkFrame = 1,
    nextPoint = (0,32),
    commandVar = cVar,
    stateVar = sVar,
    blocked = False
}

initWorldGame :: TMVar String -> TMVar TomState -> Int -> Grid -> World
initWorldGame cVar sVar l g = World {
    tom = startTom cVar sVar,
    code = ["Hello!"],
    gameState = TalkBox,
    level = l,
    grid = g,
    hover = Nothing,
    codeColor = black,
    interpThread = Nothing
}


-- Window, grid and talkbox variables --

cellSize :: Float
cellSize = 64

windowWidth, windowHeight :: Int
windowWidth = floor cellSize * 17
windowHeight = floor cellSize * 12

windowSize :: (Int, Int)
windowSize = (windowWidth, windowHeight)

xCorner, yCorner :: Float
xCorner = fromIntegral windowWidth/2
yCorner = fromIntegral windowHeight/2

lineY, lineX  :: Float
lineY = yCorner
lineX = xCorner - fromIntegral windowHeight

talkBox :: Path
talkBox =
    [(-xCorner + 50, yCorner - 50), (xCorner - 50, yCorner - 50),
    (xCorner - 50, -yCorner + 150), (-xCorner + 150, -yCorner + 150),
    (-xCorner + 120, -yCorner + 120), (-xCorner + 120, -yCorner + 150),
    (-xCorner + 50, -yCorner + 150), (-xCorner + 50, yCorner - 50)]


-- Buttons and button logic --

playGameButton :: Button
playGameButton = Button {
    middle = (0, -50),
    width = 300,
    height = 80,
    txt = "Play Coding Tom",
    txtPos = (-110,-60)}

typeButton :: Button
typeButton = Button {
    middle = (lineX-50-129.5, -lineY+30+25),
    width = 259,
    height = 50,
    txt = "Run code",
    txtPos = (lineX-150-129.5, -lineY+48)}

emptyButton :: Button
emptyButton = Button {
    middle = (lineX-30, lineY-30),
    width = 35,
    height = 35,
    txt = "X",
    txtPos = (lineX-38, lineY-40)}

gameButtons :: [Button]
gameButtons = [typeButton, emptyButton, hintButton]

okButton :: Button
okButton = Button {
    middle = (xCorner - 135, -yCorner + 205),
    width = 120,
    height = 60,
    txt = "OK!",
    txtPos = (xCorner - 135 - 50, -yCorner + 205 - 20)}

quitButton :: Button
quitButton = Button {
    middle = (xCorner - 30, yCorner - 30),
    width = 35,
    height = 35,
    txt = "Q",
    txtPos = (xCorner - 40, yCorner - 40)
}

hintButton :: Button
hintButton = Button {
    middle = (xCorner - 70, yCorner - 30),
    width = 35,
    height = 35,
    txt = "H",
    txtPos = (xCorner - 80, yCorner - 40)
}

drawButton :: Float -> Button -> Picture
drawButton greyScale button = pictures [
    uncurry translate (middle button) $ color (greyN greyScale) (rectangleSolid (width button) (height button)),
    uncurry renderText (txtPos button) (txt button)]

hovering :: Maybe Button  -> Picture
hovering (Just button) = drawButton 0.5 button
hovering Nothing = blank

getUpperCorner :: Button -> (Float, Float)
getUpperCorner button = (fst (middle button) - (width button/2), snd (middle button) + (height button/2))

getLowerCorner :: Button -> (Float, Float)
getLowerCorner button = (fst (middle button) + (width button/2), snd (middle button) - (height button/2))


-- Game logic --

getSeg :: Grid -> Point -> Char
getSeg (Grid _ g) (x,y) =
    case find (\(a,b,_) -> a == yDiv y cellSize && b == xDiv x cellSize) g of
        Just (_,_,c) -> c
        Nothing -> 'E'
        where
            yDiv a b = fromIntegral (floor $ (yCorner - a) / b)
            xDiv a b = fromIntegral (floor $ (xCorner + a) / b)

isAtGoal :: Tom -> Grid -> Bool
isAtGoal t g = getSeg g (pos t) == 'G'

levelComplete :: TMVar String -> TMVar TomState -> World -> IO World
levelComplete cVar sVar world = do
    let g = grid world

    if isAtGoal (tom world) g then do
        maybe (return ()) killThread (interpThread world)
        _ <- atomically $ tryTakeTMVar cVar
        _ <- atomically $ tryTakeTMVar sVar
        return $ initWorldGame cVar sVar (level world + 1) g

    else return world


-- Drawing the game map --

drawTom :: [Picture] -> World -> Picture
drawTom images world = translate x y $ images !! (walkFrame (tom world) `mod` length images)
  where
    (x, y) = pos (tom world)

drawGrid :: Grid -> IO Picture
drawGrid (Grid _ g) = do
    drawnG <- mapM (\(row, col, char) -> drawCell char (-xCorner+(32+col*cellSize), yCorner-(32+row*cellSize))) g
    return $ pictures drawnG

drawCell :: Char -> (Float, Float) -> IO Picture
drawCell 'W' (x,y) = do
    wallLoaded <- loadBMP wall
    return $ translate x y wallLoaded
drawCell 'E' (x,y) = return $ translate x y $ color (makeColor 0.42 0.74 1 1) $ rectangleSolid cellSize cellSize
drawCell 'C' (x,y) = return $ translate x y $ color white $ rectangleSolid cellSize cellSize
drawCell 'G' (x,y) = return $ translate x y $ color green $ rectangleSolid cellSize cellSize
drawCell _ _ = return $ error "Invalid character. A grid should only include the characters 'W', 'C', 'E', and 'G'."

indexText :: Int -> IO String -> IO Grid
indexText lvl str = do
    file <- str
    return $ Grid lvl $ concatMap (\(ind, row) -> zip3 (repeat ind) [0..] row) $ zip [0..] $ lines file


-- Handling the text --

tooLong :: String -> Bool
tooLong str = length str >= 20

nextLine :: [String] -> String -> Char -> [String]
nextLine content word c = removeLastWord content ++ [word ++ [c]]

newLine :: [String] -> [String]
newLine content = content ++ [""]

removeLastWord :: [String] -> [String]
removeLastWord content =
    init content ++ [unwords $ init $ words' $ last content]

getLastWord :: [String] -> String
getLastWord content = last $ words' $ last content

backSpace :: [String] -> [String]
backSpace [] = []
backSpace content =
    if null (last content)
        then init content
        else init content ++ [init (last content)]

drawText :: [String] -> Point -> Color -> Picture
drawText content (x,y) c = color c $ pictures (go content (x,y)) where
    go [] _ = []
    go (l:ls) (a,b) = translate a b (scale 0.2 0.2 $ text l) : go ls (a, b - 30)

renderText :: Float -> Float -> String -> Picture
renderText x y content = translate x y $ scale 0.2 0.2 $ text content


-- All grids --

loadGrid :: Int -> IO Grid
loadGrid 0 = indexText 0 $ readFile "./resources/grids/level0.txt"
loadGrid 1 = indexText 1 $ readFile "./resources/grids/level1.txt"
loadGrid 2 = indexText 2 $ readFile "./resources/grids/level2.txt"
loadGrid 3 = indexText 3 $ readFile "./resources/grids/level3.txt"
loadGrid 4 = indexText 4 $ readFile "./resources/grids/level4.txt"
loadGrid _ = indexText 0 $ readFile "./resources/grids/level0.txt"


-- Levels welcome text --

getWelcomeLevelText :: Int -> [String]
getWelcomeLevelText 1 =
    ["Hello and welcome to Coding Tom! My name is Tom, and together",
    "we will learn how to code! I need help to get through a series of",
    "labyrinths, and your code will help me to do just that! Give me",
    "instrouctions at each level on how to walk to get to the end by",
    "writing simple code for me to read.",
    "",
    "Lets start simple. For this first labyrinth I just need to take a few",
    "steps forward to get to the end, so simply just ask me to walk 5",
    "times. Type 'walk' in the code editor on the left side of the screen",
    "to get me to walk the lenght of one unit. You can choose if you",
    "want to type it with a space between each command, or write each",
    "command in a new line.",
    "",
    "Psst! This is one unit"
    ]    
getWelcomeLevelText 2 =
    ["That was great! You have just written your first few lines of code,",
    "and we are already on to level 2! For this next level I see I need",
    "to take a turn to the left at one point. This is easy though, just",
    "type 'turnLeft', and I'll turn left. Don't forget to wirte the walking",
    "commands too, and remember to write the turning command at",
    "the right place. You got this!"
    ]   
getWelcomeLevelText 3 = 
    ["Yay, time for level 3! So, level 3 is actually the same map as",
    "level 1 - let me explain. You're getting really good at writing",
    "code for me to reach the finish line, but writing 'walk' over and",
    "over again is kind of insufficient, don't you think? This is where",
    "loops get handy. Loops are just a few lines of code ran over and",
    "over again. Just what we need! In our case, when you wirte a loop",
    "you start with the keyword 'while', followed by a state I may be in,",
    "like 'blocked' or 'notBlocked'. Then you wirte a start parens, and",
    "the code that you want to be repeated, which in this case is 'walk',",
    "and end with an end parens. So, a while loop is like saying 'as",
    "long as Tom is in this state, do this repeatedly'. This is the",
    "outline of the while loop:",
    "while ___ (___)",
    "",
    "Let's try this! Write a while loop for me to walk as long as I am",
    "not finished with the level. The state for this is 'notFinished'.",
    "Good Luck!"
    ] 
getWelcomeLevelText 4 = 
    ["Amazing work! Let's move on to level 4. Okay, you have now learnt",
    "about loops which is a great way to do something several times.",
    "However, as you'll see when we start this next level, we need to do a",
    "turn to the right at some point. So, writing a while loop like the",
    "last one won't work. We need to incorporate a turn into the loop in",
    "some way. This is where if-statements come in handy. An if-statement",
    "is basically saying 'if something is true, do this. If not, do this'.",
    "You write an if-statement like this: you start with the keyword 'if',",
    "followed by a state I might be in, like 'blocked' or 'notBlocked'.",
    "Then you write a command, which in this case is 'turnRight'. Lastly,",
    "you write the keyword 'else', followed by another command that will be",
    "done if the state is not true. Makes sense?",
    "",
    "Now, let's try this! Write a while loop with an if-statement in it.",
    "Don't remember where to put the parens? This is the outline:",
    "while ___ (if ___ (___) else (___))"
    ]
getWelcomeLevelText 5 = ["Done with the game!"]
getWelcomeLevelText _ = error "Invalid level number, expected integer between 0 and 4"


-- Helper functions -- 

head' :: [String] -> String
head' [] = ""
head' [a] = a
head' (x:y:_) = x ++ y

last' :: [String] -> String
last' [] = ""
last' xs = last xs

init' :: [String] -> [String]
init' [] = []
init' xs = init xs

words' :: String -> [String]
words' str
    | last str == ' ' = words str ++ [""]
    | otherwise = words str
