module Main where

import Control.Monad (filterM)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.Foldable (Foldable(toList))

main :: IO ()
main = do
  input <- readInput
  part1 input
  part2 input


{-
-}
part1 :: Maybe [Int] -> IO ()
part1 = printMaybe "Part 1: " . fmap (countUpticks 1)

{-
 This part asks to compare differences in sums across sliding windows
 of size 3, which is equivalent to comparing x_i to x_{i+3} in a sequence.

 E.g.: We can compare 199, 210 here to compare window A to B, 200 and 222 for
 B v/s C. Instead of summing up the respsective subsequences
 199  A
 200  A B
 208  A B C
 210    B C
 222      C

-}
part2 :: Maybe [Int] -> IO ()
part2 = printMaybe "Part 2: " . fmap (countUpticks 3)

readInput :: IO (Maybe [Int])
readInput = readIntSequence <$> getContents

dup :: a -> (a, a)
dup xs = (xs, xs)

fdot :: (a -> b, c -> d) -> (a, c) -> (b, d)
fdot (f, g) (xs, ys) = (f xs, g ys)

printMaybe :: (Show a) => String -> Maybe a -> IO ()
printMaybe banner (Just x) = putStrLn $ banner ++ show x
printMaybe banner x = putStrLn $ banner ++ show x

readInt :: String -> Maybe Int
readInt = readMaybe

readIntSequence :: String -> Maybe [Int]
readIntSequence = sequence . filter isJust . map readInt . lines

countUpticks :: Int -> [Int] -> Int
countUpticks n nums = length $ filter (uncurry (<)) $ zip nums (drop n nums)

