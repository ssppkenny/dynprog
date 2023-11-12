module Main (main) where

import Lib

import Data.Maybe ( catMaybes , isJust)
import Data.List ( sort )
import qualified Data.Map as Map
import Data.Function.Memoize


coins = [1,2,5]


changeHelper :: Int -> [Int] -> Maybe [Int]
changeHelper 0 acc = Just acc
changeHelper amount acc
  | amount < minimum coins = Nothing
  | otherwise = Just best_solution
  where
      solutions = [changeHelper (amount - coin) (sort (coin:acc)) | coin <- coins]
      non_empty_solutions = catMaybes solutions      
      best_solution
        = foldr1 (\ x y -> if length x < length y then x else y) non_empty_solutions


mchangeHelper = memoize2 changeHelper


change :: Int  -> Maybe [Int]
change amount = mchangeHelper amount []



fib' n = if n == 1 then (1, Map.fromList [(1, 1)])
                   else if n == 2 then (1, Map.fromList [(1,1), (2,1)])
                   else let 
                    n1 = fib' (n-1)
                    r1 = fst n1 
                    m1 = snd n1
                    n2 = fib' (n-2)
                    r2 = fst n2 
                    m2 = snd n2
                    m = Map.insert n (r1 + r2) (Map.union m1 m2)
                    in (r1 + r2, m)

fib n = let 
    r = fib' n
    m = snd r 
    in m Map.! n


memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)


memoized_change :: Int -> Maybe [Int]
memoized_change = (map changeHelper' [0..] !!)
    where changeHelper' 0 = Just []
          changeHelper' amount = if amount < minimum coins then Nothing 
                                                        else let 
                                                            solutions = [(c, memoized_change (amount-c)) | c <- coins, amount - c >= 0]
                                                            results = filter (\a -> (isJust (snd a))) solutions
                                                            rs = map (\p -> addToList (fst p) (snd p)) results
                                                            non_empty_solutions = catMaybes rs
                                                                    in Just $ foldr1 (\x y -> if length x < length y then x else y) non_empty_solutions
                    



main :: IO ()
main = someFunc
