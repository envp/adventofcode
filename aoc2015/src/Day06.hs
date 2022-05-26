module Main (main) where

import Control.Monad (void)
import qualified Data.Set as S
import Parsing (eol, parse, readInteger)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, optional, skipSpaces, string, (+++))

-- X, Y coords
type Point = (Int, Int)

-- Defined by top-left and bottom right corners
type Rectangle = (Point, Point)

data Action = TurnOn | TurnOff | Toggle deriving (Show)

main :: IO ()
main = do
  rawLines <- lines <$> getContents
  data <- parse inputP rawLines

inputP :: ReadP (Action, Rectangle)
inputP = do
  let onP = string "on" >> return TurnOn
  let offP = string "off" >> return TurnOn
  action <-
    ( string "turn o"
        >> ( (string "n" >> return TurnOn)
               +++ (string "ff" >> return TurnOff)
           )
      )
      +++ ( string "toggle" >> return Toggle
          )
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
  return (action, ((x0, y0), (x1, y1)))
