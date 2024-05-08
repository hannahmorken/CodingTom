module GameCore where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import ImageLoader

import Data.List (find)


-- Core data types in the game --

data Tom = Tom {
    pos :: Point,
    dir :: Direction,
    img :: FilePath,
    walkFrame :: Int,
    endPoint :: Point,
    commandQueue :: [String]
} deriving (Eq)

data World = World {
    tom :: Tom,
    code :: [String],
    gameState :: GameState,
    level :: Int,
    grid :: Grid,
    hover :: Maybe Button,
    codeColor :: Color
}

data Direction = North | South | West | East deriving (Eq)

data GameState = Menu | Game | TalkBox

data Grid = Grid Int [(Float, Float, Char)]

data Button = Button {
    middle :: Point,
    height :: Float,
    width :: Float,
    txt :: String,
    txtPos :: Point
}


-- Initial menu and game world --

initWorld :: World
initWorld = World {
    tom = Tom {pos = (0,100), dir = East, img = tomStraight, walkFrame = 1, endPoint = (0,100), commandQueue = []},
    code = [],
    gameState = Menu,
    level = 1,
    grid = Grid 0 [],
    hover = Nothing,
    codeColor = black
}

startTom :: Tom
startTom = Tom {
    pos = (0,32),
    dir = East,
    img = tomStandRight,
    walkFrame = 1,
    endPoint = (0,32),
    commandQueue = []
}

initWorldGame :: Int -> Grid -> World
initWorldGame level grid = World {
    tom = startTom,
    code = ["Hello!"],
    gameState = TalkBox,
    level = level,
    grid = grid,
    hover = Nothing,
    codeColor = black
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
    middle = (0, 0),
    width = 330,
    height = 50,
    txt = "This is a game menu",
    txtPos = (-150,-10)}

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
gameButtons = [typeButton, emptyButton]

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
    txtPos = (xCorner - 38, yCorner - 40)
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
getSeg (Grid name grid) (x,y) =
    case find (\(a,b,_) -> a == yDiv y cellSize && b == xDiv x cellSize) grid of
        Just (a,b,c) -> c
        Nothing -> 'E'
        where
            yDiv x y = fromIntegral (floor $ (yCorner - x) / y)
            xDiv x y = fromIntegral (floor $ (xCorner + x) / y)

isAtGoal :: Tom -> Grid -> Bool
isAtGoal tom grid = getSeg grid (pos tom) == 'G'

hasStopped :: World -> Bool
hasStopped world = 
    let tom' = tom world in
    walkFrame tom' == 1 &&
    null (commandQueue tom') &&
    not (isAtGoal tom' (grid world)) &&
    tom' /= startTom

wrongCode :: World -> World
wrongCode world =
    if hasStopped world
        then world {code = []}
        else world

levelComplete :: World -> World
levelComplete world =
    let grid' = grid world in
    if isAtGoal (tom world) grid'
        then initWorldGame (level world + 1) grid'
    else world

-- Drawing the game map --

drawTom :: [Picture] -> World -> Picture
drawTom images world = translate x y $ images !! (walkFrame (tom world) `mod` length images)
  where
    (x, y) = pos (tom world)

getWelcomeLevelText :: Int -> [String]
getWelcomeLevelText 2 =
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
    "Psst! This is one unit"]
getWelcomeLevelText 1 =
    ["That was great! You have just written your first few lines of code,",
    "and we are already on to level 2! For this next level I see I need",
    "to take a turn to the left at one point. This is easy though, just",
    "type 'turn left', and I'll turn left. Remember to write the turning",
    "command at the right place. Good luck!"
    ]


drawGrid :: Grid -> IO Picture
drawGrid (Grid name grid) = do
    grid' <- mapM (\(row, col, char) -> drawCell char (-xCorner+(32+col*cellSize), yCorner-(32+row*cellSize))) grid
    return $ pictures grid'

drawCell :: Char -> (Float, Float) -> IO Picture
drawCell 'W' (x,y) = do
    wall <- loadBMP wall
    return $ translate x y wall
drawCell 'E' (x,y) = return $ translate x y $ color (makeColor 0.42 0.74 1 1) $ rectangleSolid cellSize cellSize
drawCell 'C' (x,y) = return $ translate x y $ color white $ rectangleSolid cellSize cellSize
drawCell 'G' (x,y) = return $ translate x y $ color green $ rectangleSolid cellSize cellSize


indexText :: Int -> IO String -> IO Grid
indexText lvl file = do
    text <- file
    return $ Grid lvl $ concatMap (\(ind, row) -> zip3 (repeat ind) [0..] row) $ zip [0..] $ lines text


-- Handling the text --

tooLong :: String -> Bool
tooLong str = length str >= 20

nextLine :: [String] -> String -> Char -> [String]
nextLine code word c = removeLastWord code ++ [word ++ [c]]

newLine :: [String] -> [String]
newLine code = code ++ [""]

removeLastWord :: [String] -> [String]
removeLastWord code =
    init code ++ [unwords $ init $ words' $ last code]

getLastWord :: [String] -> String
getLastWord code = last $ words' $ last code

backSpace :: [String] -> [String]
backSpace [] = []
backSpace code =
    if null (last code)
        then init code
        else init code ++ [init (last code)]

splitText :: [String] -> Int -> [String] -> [String]
splitText lines a [] = lines
splitText [] a (word:words) = splitText [word] (length word) words
splitText lines a (word:words) =
    if a + length word + 1 <= 10
        then splitText (init lines ++ [last lines ++ " " ++ word]) (a + length word + 1) words
        else lines ++ splitText [word] 0 words

drawText :: [String] -> Point -> Color -> Picture
drawText lines (x,y) c = color c $ pictures (go lines (x,y)) where
    go [] _ = []
    go (l:ls) (x,y) = translate x y (scale 0.2 0.2 $ text l) : go ls (x, y - 30)

renderText :: Float -> Float -> String -> Picture
renderText x y txt = translate x y $ scale 0.2 0.2 $ text txt

-- All grids --

loadGrid :: Int -> IO Grid
loadGrid 1 = indexText 1 $ readFile "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/grids/level1.txt"
loadGrid 2 = indexText 2 $ readFile "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/grids/level2.txt"


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
words' line
    | last line == ' ' = words line ++ [""]
    | otherwise = words line