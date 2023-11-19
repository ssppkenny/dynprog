{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main
  ( main
  , memoizedChange
  , initMap
  , solve
  , dynSolve
  , findSubset
  , findSubsets
  ) where

import Lib

import Control.Monad.Reader
import Data.IORef
import Data.List (findIndices, partition)
import Data.List.Split (splitWhen)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)

coins :: [Int]
coins = [1, 2, 5]

type MyMap = M.Map Int Int

type DynMap = M.Map Int [Int]

findSubset :: Int -> [Int] -> Maybe [Int]
findSubset s lst =
  case lst of
    [] -> Nothing
    [_] -> Nothing
    n:ns ->
      if n == s
        then Just [n]
        else if n < s
               then case findSubset (s - n) ns of
                      Just t -> Just (n : t)
                      Nothing -> findSubset s ns
               else findSubset s ns

findSubsets :: [Int] -> Maybe ([Int], [Int])
findSubsets lst =
  if even (sum lst)
    then case findSubset (div (sum lst) 2) lst of
           Just l -> Just $ partition (`elem` l) lst
           Nothing -> Nothing
    else Nothing

mergeMaps :: [DynMap] -> DynMap
mergeMaps lst =
  case lst of
    [] -> M.empty
    _ ->
      foldl1
        (M.unionWith
           (\a b ->
              if length a > length b
                then a
                else b))
        lst

initMap :: DynMap
initMap = M.fromList [(0, [])]

solve :: Int -> Int -> DynMap -> [Int] -> DynMap
solve amount paid solutions coins =
  if paid < amount
    then case M.lookup paid solutions of
           Just _ ->
             let mergedSolutions =
                   mergeMaps
                     [ M.insert
                       (paid + c)
                       ((solutions M.! paid) ++ [c])
                       solutions
                     | c <- coins
                     , paid + c <= amount
                     , isNothing (M.lookup (paid + c) solutions)
                         || length (M.lookup (paid + c) solutions)
                              > length (M.lookup paid solutions) + 1
                     ]
              in solve
                   amount
                   (paid + 1)
                   (if null mergedSolutions
                      then solutions
                      else mergedSolutions)
                   coins
           Nothing -> solve amount (paid + 1) solutions coins
    else solutions

dynSolve :: Int -> [Int] -> [Int]
dynSolve amount coins = solve amount 0 initMap coins M.! amount

shortestSolution :: [[Int]] -> [Int]
shortestSolution =
  foldr1
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

findMaxSubListSum :: [Int] -> Int
findMaxSubListSum lst =
  case lst of
    [] -> 0
    _ ->
      let loopiter lst chunks =
            case lst of
              [] -> chunks
              x:xs ->
                if not (null chunks) && last chunks < 0
                  then loopiter xs (chunks ++ [x])
                  else if null chunks
                         then loopiter xs [x]
                         else loopiter xs (chunks ++ [last chunks + x])
       in maximum $ loopiter lst []

findMaxSubListProd' :: ([Int], Int) -> Int
findMaxSubListProd' p =
  if even $ snd p
    then product $ fst p
    else max (product $ findLeft' $ fst p) (product $ findRight' $ fst p)

countNegs lst' = length $ findIndices (< 0) lst'

findMaxSubListProd :: [Int] -> Int
findMaxSubListProd lst =
  let splitByZero = splitWhen (== 0) lst
      pairs = [(x, countNegs x) | x <- splitByZero]
   in maximum [findMaxSubListProd' x | x <- pairs]

findLeft' :: [Int] -> [Int]
findLeft' lst =
  let inds = findIndices (< 0) lst
      pos = last inds
   in take pos lst

findRight' :: [Int] -> [Int]
findRight' lst =
  let inds = findIndices (< 0) lst
      pos = head inds
   in take pos lst

changeReader :: Reader (Int, [Int]) [Int]
changeReader = do
  env <- ask
  case env of
    (amount, coins) ->
      let memoizedChange' = (map change [0 ..] !!)
            where
              change 0 = Just []
              change amount =
                if amount < minimum coins
                  then Nothing
                  else let solutions =
                             [ (c, memoizedChange' (amount - c))
                             | c <- coins
                             , amount - c >= 0
                             ]
                           results = filter (isJust . snd) solutions
                           rs = map (uncurry addToList) results
                           non_empty_solutions = catMaybes rs
                        in Just $ shortestSolution non_empty_solutions
       in return $ fromMaybe [] (memoizedChange' amount)

main :: IO ()
main = do
  numRef <- newIORef @MyMap M.empty
  current <- readIORef numRef
  let newMap = M.insert 1 100 current
  writeIORef numRef newMap
  next <- readIORef numRef
  let a = next M.! 1
  let res = findSubsets [2, 3, 5, 6]
  print $ show res
