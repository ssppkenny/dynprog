module Lib
    ( someFunc, addToList
    ) where

import Data.List ( sort )

addToList :: Int -> Maybe [Int] -> Maybe [Int]
addToList n lst = case lst of
    Nothing -> Nothing
    Just l -> Just $ sort (n : l)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

