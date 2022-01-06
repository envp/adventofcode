module Main where

import Data.List (transpose)

data Bit
  = F
  | T
  deriving (Show, Eq)

main :: IO ()
main = do
  input <- readInputLines
  part1 input
  part2 input

part2 :: [[Bit]] -> IO ()
part2 input = putStrLn $ "Part 2: " ++ show (answer input)
  where
    answer = lifeSupportRating

lifeSupportRating :: [[Bit]] -> Int
lifeSupportRating xss = o2Rating xss * co2Rating xss

o2Rating :: [[Bit]] -> Int
o2Rating = o2Rating' 0
  where
    o2Rating' _ [xs] = bin2dec xs
    o2Rating' n xss = o2Rating' (n + 1) xss'
      where
        xss' = filter (\xs -> bit == xs !! n) xss
        bit = mostFrequent (transpose xss !! n)

co2Rating :: [[Bit]] -> Int
co2Rating = co2Rating' 0
  where
    co2Rating' _ [xs] = bin2dec xs
    co2Rating' n xss = co2Rating' (n + 1) xss'
      where
        xss' = filter (\xs -> bit == xs !! n) xss
        bit = invert . mostFrequent $ transpose xss !! n

part1 :: [[Bit]] -> IO ()
part1 input = putStrLn $ "Part 1: " ++ show (answer input)
  where
    answer = powerConsumption . map mostFrequent . transpose

powerConsumption :: [Bit] -> Int
powerConsumption xs = computeGamma xs * computeEpsilon xs

bitToInt :: Bit -> Int
bitToInt F = 0
bitToInt T = 1

invert :: Bit -> Bit
invert T = F
invert F = T

bin2dec :: [Bit] -> Int
bin2dec = foldl1 (\acc x -> 2 * acc + x) . map bitToInt

computeGamma :: [Bit] -> Int
computeGamma = bin2dec

computeEpsilon :: [Bit] -> Int
computeEpsilon = bin2dec . map invert

-- Assumes that only '1', '0' appear in the input
mostFrequent :: [Bit] -> Bit
mostFrequent xs = if total >= 0 then T else F
  where
    values = map (\x -> if x == T then 1 else -1) xs
    total :: Int
    total = sum values

charToBit :: Char -> Bit
charToBit '1' = T
charToBit '0' = F
charToBit _ = error "Unknown"

readInputLines :: IO [[Bit]]
readInputLines = map (map charToBit) . filter (not . null) . lines <$> getContents
