module Main
  ( main
  , memoizedChange) where

import Lib

import Data.Maybe (catMaybes, isJust)

coins :: [Int]
coins = [1, 2, 5]

shortestSolution :: [[Int]] -> [Int]
shortestSolution = foldr1
    (\x y ->
       if length x < length y
         then x
         else y)

memoizedChange :: Int -> Maybe [Int]
memoizedChange = (map change [0 ..] !!)
  where
    change 0 = Just []
    change amount =
      if amount < minimum coins
        then Nothing
        else let solutions =
                   [ (c, memoizedChange (amount - c))
                   | c <- coins
                   , amount - c >= 0
                   ]
                 results = filter (isJust . snd) solutions
                 rs = map (uncurry addToList) results
                 non_empty_solutions = catMaybes rs
              in Just $ shortestSolution non_empty_solutions

main :: IO ()
main = someFunc
