module Main (main) where

import Data.List (sort)
import Parsing (eol, parse, readInteger)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    sepBy1,
    skipSpaces,
  )

type Dimensions = (Int, Int, Int)

sortDims :: Dimensions -> Dimensions
sortDims (a, b, c) = (d !! 0, d !! 1, d !! 2)
  where
    d = sort [a, b, c]

main :: IO ()
main = do
  rawInput <- getContents
  let dimensions = parse parseInput rawInput
  putStrLn $ "Part 1: " ++ (show . sum . map paperArea) dimensions
  putStrLn $ "Part 2: " ++ (show . sum . map ribbonLength) dimensions
  where
    paperArea :: Dimensions -> Int
    paperArea dims@(l, w, h) =
      let surfaceArea = 2 * (l * w + w * h + h * l)
          (d0, d1, _) = sortDims dims
          slack = d0 * d1
       in surfaceArea + slack

    ribbonLength :: Dimensions -> Int
    ribbonLength dims =
      let (d0, d1, d2) = sortDims dims
       in 2 * (d0 + d1) + d0 * d1 * d2

parseInput :: ReadP [Dimensions]
parseInput = do
  skipSpaces
  dims <- sepBy1 readDims eol
  skipSpaces
  eof
  return dims
  where
    readDims :: ReadP Dimensions
    readDims = do
      d0 <- readInteger
      _ <- char 'x'
      d1 <- readInteger
      _ <- char 'x'
      d2 <- readInteger
      return (d0, d1, d2)
