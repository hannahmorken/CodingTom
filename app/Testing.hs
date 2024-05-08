module Testing where
import Test.QuickCheck
import Data.List (intersperse)
import Interpreter
import GameCore
import Control.Monad.State
import Text.Megaparsec

prop_cmds :: String -> Bool
prop_cmds input = 
    let cmds = case parse statement "" input of
            Left _ -> CmdList []
            Right cmdList -> cmdList
        cmdsIndv = foldl (\cmd -> 
            case parse statement "" input of
                Left _ -> CmdList []
                Right cmdList -> cmdList) True input
    in cmds == cmdsIndv


split :: Char -> String -> [String]
split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]

-- show
prop_split_inv xs
    = forAll (elements xs) $ \c -> 
      unsplit c (split c xs) == xs

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys
