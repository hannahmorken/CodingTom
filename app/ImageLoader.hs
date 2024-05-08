module ImageLoader where
import Graphics.Gloss

-- Segments --

wall :: FilePath
wall = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/wall.bmp"

sky :: FilePath
sky = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/Untitled-4-2.bmp"

-- Tom's sprites --

tomStraight :: FilePath
tomStraight = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tom.bmp"

tomStandRight :: FilePath
tomStandRight = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tomRightStill.bmp"

tomStandLeft :: FilePath
tomStandLeft = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tomLeftStill.bmp"

tomWalkLeft1 :: FilePath
tomWalkLeft1 = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tomLeft1.bmp"

tomWalkLeft2 :: FilePath
tomWalkLeft2 = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tomLeft2.bmp"

tomWalkRight1 :: FilePath
tomWalkRight1 = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tomRight1.bmp"

tomWalkRight2 :: FilePath
tomWalkRight2 = "/Users/hannahmorken/Desktop/20232024/INF221/CodingTom/resources/tom/tomRight2.bmp"

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

