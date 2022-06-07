module Main (main) where

import Control.Monad (foldM, void)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Parsing (eol, parse, readInteger)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, skipSpaces, string, (<++))

-- X, Y coords
type Point = (Int, Int)

-- Defined by top-left and bottom right corners
type Rectangle = (Point, Point)

data Action = TurnOn | TurnOff | Toggle deriving (Show)

main :: IO ()
main = do
  rawLines <- lines <$> getContents
  let instructions = map (parse inputP) rawLines
  let part1 = length $ foldl' p1 S.empty instructions
  let part2 = sum . M.elems $ foldl' p2 M.empty instructions
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
  where
    enumeratePoints :: Rectangle -> [Point]
    enumeratePoints ((x0, y0), (x1, y1)) = [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]]

    toggle :: S.Set Point -> Point -> S.Set Point
    toggle s p =
      if S.member p s
        then S.delete p s
        else S.insert p s

    p1Act :: Action -> S.Set Point -> Point -> S.Set Point
    p1Act TurnOn s p = S.insert p s
    p1Act TurnOff s p = S.delete p s
    p1Act Toggle s p = if S.member p s then S.delete p s else S.insert p s

    p2Act :: Action -> M.Map Point Int -> Point -> M.Map Point Int
    p2Act TurnOn m p = M.alter (maybe (Just 1) (\v -> Just (v + 1))) p m
    p2Act TurnOff m p = M.alter (maybe (Just 0) (\v -> Just (max 0 (v - 1)))) p m
    p2Act Toggle m p = M.alter (maybe (Just 2) (\v -> Just (v + 2))) p m

    p1 :: S.Set Point -> (Rectangle, Action) -> S.Set Point
    p1 state (rect, action) = foldl' (p1Act action) state (enumeratePoints rect)

    p2 :: M.Map Point Int -> (Rectangle, Action) -> M.Map Point Int
    p2 state (rect, action) = foldl' (p2Act action) state (enumeratePoints rect)

inputP :: ReadP (Rectangle, Action)
inputP = do
  action <-
    choice
      [ string "turn on" >> return TurnOn,
        string "turn off" >> return TurnOff,
        string "toggle" >> return Toggle
      ]
  skipSpaces
  x0 <- readInteger
  _ <- char ','
  y0 <- readInteger
  skipSpaces
  _ <- string "through"
  skipSpaces
  x1 <- readInteger
  _ <- char ','
  y1 <- readInteger
  _ <- void eol <++ eof
  return (((x0, y0), (x1, y1)), action)
