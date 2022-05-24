module Main (main) where

import Control.Arrow ((***))
import Data.List (group, isInfixOf, sort)

main :: IO ()
main = do
  rawLines <- lines <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 rawLines)
  putStrLn $ "Part 2: " ++ show (part2 rawLines)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

everyOther :: [a] -> ([a], [a])
everyOther (x : y : xs) = (x :) *** (y :) $ everyOther xs
everyOther (x : xs) = (x :) *** id $ everyOther xs
everyOther [] = ([], [])

part1 :: [String] -> Int
part1 xss =
  let hasAtleast3Vowels :: [Bool]
      hasAtleast3Vowels = map ((>= 3) . length . filter (`elem` "aeiou")) xss

      hasRepeatedLetter :: [Bool]
      hasRepeatedLetter = map (any (uncurry (==)) . pairs) xss

      lacksDisallowedChars :: [Bool]
      lacksDisallowedChars = map (\xs -> not (any (`isInfixOf` xs) ["ab", "cd", "pq", "xy"])) xss
   in length $
        filter id $
          zipWith3 (\a b c -> a && b && c) hasAtleast3Vowels hasRepeatedLetter lacksDisallowedChars

part2 :: [String] -> Int
part2 xss =
  let hasRepeatedPairs :: [Bool]
      hasRepeatedPairs = map (any ((> 1) . length) . group . sort . pairs) xss

      hasSkippedRepeats :: [Bool]
      hasSkippedRepeats = map (or . uncurry (zipWith (==)) . everyOther) xss
   in length $
        filter id $ zipWith (==) hasRepeatedPairs hasSkippedRepeats
