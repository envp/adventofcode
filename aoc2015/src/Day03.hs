module Main (main) where

import Data.List (union)

data Direction = U | D | L | R deriving (Show)

type Point = (Int, Int)

origin :: Point
origin = (0, 0)

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
  where
    part1 :: [Direction] -> Int
    part1 ds = length $ union [] (genPositions ds)

    part2 :: [Direction] -> Int
    part2 ds =
      let splitEvenOdd = foldr (\d ~(dEven, dOdd) -> (d : dOdd, dEven)) ([], [])
          (santa, robo) = splitEvenOdd ds
          uniqueSantaPts = union [] (genPositions santa)
          uniqueRoboPts = union [] (genPositions robo)
          uniquePts = union uniqueRoboPts uniqueSantaPts
       in length uniquePts

    genPositions :: [Direction] -> [Point]
    genPositions = scanl (flip move) origin

    move :: Direction -> Point -> Point
    move U (x, y) = (x, y + 1)
    move D (x, y) = (x, y - 1)
    move L (x, y) = (x + 1, y)
    move R (x, y) = (x - 1, y)

readInput :: IO [Direction]
readInput = getContents >>= (return . map charToDirection . filter (`elem` "^v><"))
  where
    charToDirection c = case c of
      '^' -> U
      'v' -> D
      '>' -> R
      '<' -> L
      _ -> error $ "Unknown char: " ++ show c
