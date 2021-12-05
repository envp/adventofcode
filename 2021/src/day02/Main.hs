module Main where

import Data.List (isPrefixOf)

newtype Aim = Aim Int

newtype Position = Position (Int, Int)

data Command = Forward Int | Up Int | Down Int deriving (Show)

main :: IO ()
main = do
  input <- readInput
  part1 input
  part2 input

printBanner :: (Show a) => String -> a -> IO ()
printBanner arg1 arg2 = putStrLn (arg1 ++ show arg2)

part1 :: [Command] -> IO ()
part1 = printBanner "Part 1: " . multiplyXY . foldl reducer (Position (0, 0))
  where
    reducer :: Position -> Command -> Position
    reducer (Position (x, y)) (Forward dx) = Position (x + dx, y)
    reducer (Position (x, y)) (Up dy) = Position (x, y - dy)
    reducer (Position (x, y)) (Down dy) = Position (x, y + dy)

part2 :: [Command] -> IO ()
part2 = printBanner "Part 2: " . multiplyAXY . foldl reducer (Aim 0, Position (0, 0))
  where
    reducer :: (Aim, Position) -> Command -> (Aim, Position)
    reducer (Aim a, Position (x, y)) (Forward dx) = (Aim a, Position (x + dx, y + a * dx))
    reducer (Aim x, pos) (Up d) = (Aim $ x - d, pos)
    reducer (Aim x, pos) (Down d) = (Aim $ x + d, pos)

multiplyXY :: Position -> Int
multiplyXY (Position (x, y)) = x * y

multiplyAXY :: (a, Position) -> Int
multiplyAXY (_, Position (x, y)) = x * y

parseCommand :: String -> Command
parseCommand cmd
  | "forward " `isPrefixOf` cmd = Forward $ parseFwd cmd
  | "up " `isPrefixOf` cmd = Up $ parseUp cmd
  | "down " `isPrefixOf` cmd = Down $ parseDown cmd
  | otherwise = error ("Unknown command: " ++ cmd)
  where
    readInt xs = read xs :: Int
    parseFwd = readInt . drop 8
    parseUp = readInt . drop 3
    parseDown = readInt . drop 5

readInput :: IO [Command]
readInput = map parseCommand . lines <$> getContents
