module Main (main) where

import Lib

import Data.Maybe ( catMaybes , isJust)
import Data.List ( sort )
import qualified Data.Map as Map
import Data.Function.Memoize


coins = [1,2,5]

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

shortest_solution :: [[Int]] -> [Int]
shortest_solution lst = foldr1 (\x y -> if length x < length y then x else y) lst

memoized_change :: Int -> Maybe [Int]
memoized_change = (map changeHelper' [0..] !!)
    where changeHelper' 0 = Just []
          changeHelper' amount = if amount < minimum coins then Nothing 
                                                        else let 
                                                            solutions = [(c, memoized_change (amount-c)) | c <- coins, amount - c >= 0]
                                                            results = filter (\a -> (isJust (snd a))) solutions
                                                            rs = map (\(x,y)-> addToList x y) results
                                                            non_empty_solutions = catMaybes rs
                                                                    in Just $ shortest_solution non_empty_solutions
                    
main :: IO ()
main = someFunc
