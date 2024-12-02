{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Text (pack, splitOn, unpack)

task1 :: IO ()
task1 = do
  input <- readFile "input"
  let nums :: [[Integer]] = map (map (read . unpack) . splitOn "   " . pack) (lines input)
  let left :: [Integer] = sort $ map head nums
  let right :: [Integer] = sort $ map (head . tail) nums
  let res :: Integer = sumRes left right
  print $ "Task 1 Result: " ++ show res

task2 :: IO ()
task2 = do
  input <- readFile "input"
  let nums :: [[Integer]] = map (map (read . unpack) . splitOn "   " . pack) (lines input)
  let left :: [Integer] = sort $ map head nums
  let right :: [Integer] = sort $ map (head . tail) nums
  let res = sum $ map (\x -> x * score right x) left
  print $ "Task 2 Result: " ++ show res

score :: [Integer] -> Integer -> Integer
score (y : ys) x
  | null ys && x == y = 1
  | null ys = 0
  | y == x = 1 + score ys x
  | otherwise = score ys x

sumRes :: [Integer] -> [Integer] -> Integer
sumRes (x : xs) (y : ys)
  | null xs = diff
  | otherwise = diff + sumRes xs ys
  where
    diff = abs (x - y)
