module ImageLoader where
import Graphics.Gloss


-- Segments --

wall :: FilePath
wall = "./resources/segments/wall.bmp"

sky :: FilePath
sky = "./resources/segments/sky.bmp"


-- Tom's sprites --

tomStraight :: FilePath
tomStraight = "./resources/tom/tom.bmp"

tomStandRight :: FilePath
tomStandRight = "./resources/tom/tomRightStill.bmp"

tomStandLeft :: FilePath
tomStandLeft = "./resources/tom/tomLeftStill.bmp"

tomWalkLeft1 :: FilePath
tomWalkLeft1 = "./resources/tom/tomLeft1.bmp"

tomWalkLeft2 :: FilePath
tomWalkLeft2 = "./resources/tom/tomLeft2.bmp"

tomWalkRight1 :: FilePath
tomWalkRight1 = "./resources/tom/tomRight1.bmp"

tomWalkRight2 :: FilePath
tomWalkRight2 = "./resources/tom/tomRight2.bmp"

loadRightImages :: IO [Picture]
loadRightImages = mapM loadBMP [
    tomWalkRight1,
    tomStandRight,
    tomWalkRight2]

loadLeftImages :: IO [Picture]
loadLeftImages = mapM loadBMP [
    tomWalkLeft1,
    tomStandLeft,
    tomWalkLeft2]
