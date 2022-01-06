module Main where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (find, findIndex, maximumBy, transpose)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    count,
    eof,
    many1,
    munch1,
    optional,
    readP_to_S,
    sepBy1,
    skipMany1,
    (<++),
  )

import Iterables (enumerate)

type Draws = [Int]

type Board = [[Int]]

type Marks = [[Bool]]

main :: IO ()
main = do
  input <- getContents
  let (draws, boards) = complete inputP input
  part1 draws boards
  part2 draws boards

part2 :: Draws -> [Board] -> IO ()
part2 draws boards = do
  -- For all draws, enumerate over the boards. (nBoards x nDraws)
  let gameStates = map (playBoard draws) boards
  -- For each board, the first index where that board wins
  let winningRounds = map (findIndex isWinner) gameStates
  -- Winner is the last board to win
  let (wrow, wcol) = maximumBy (compare `on` snd) $ enumerate winningRounds
  -- Assume there is always a winning column
  let winner = (gameStates !! wrow) !! (fromJust wcol)
  putStrLn $ "Part 2: " ++ show (score winner)

part1 :: Draws -> [Board] -> IO ()
part1 draws boards = do
  -- For all boards, a sequence of states by draw (nDraws x nBoards)
  let gameStates = transpose $ map (playBoard draws) boards
  let winningRound = findFirst (any isWinner) gameStates
  let winner = findFirst isWinner winningRound
  putStrLn $ "Part 1: " ++ show (score winner)
  where
    findFirst p xs = fromJust (find p xs)

score :: (Board, Marks, Int) -> Int
score (board, state, move) = move * (sum . map sum $ mask board state)
  where
    mask = zipWith (zipWith (\b s -> if s then 0 else b))

isWinner :: (Board, Marks, Int) -> Bool
isWinner (_, s, _) = hasWinningRow s || hasWinningCol s
  where
    hasWinningRow = any and
    hasWinningCol = any and . transpose

playBoard :: Draws -> Board -> [(Board, Marks, Int)]
playBoard draws board = scanl play (board, initialState, -1) draws

play :: (Board, Marks, Int) -> Int -> (Board, Marks, Int)
play (board, state, _) move = (board, markBoard board move `updateState` state, move)

updateState :: Marks -> Marks -> Marks
updateState = zipWith (zipWith (||))

markBoard :: Board -> Int -> Marks
markBoard board n = map (map (== n)) board

initialState :: Marks
initialState = replicate 5 (replicate 5 False)

parseInt :: String -> Int
parseInt = read

eol :: ReadP ()
eol = () <$ char '\n'

whitespace :: ReadP ()
whitespace = skipMany1 (char ' ')

integerP :: ReadP Int
integerP = parseInt <$> munch1 isDigit

drawsP :: ReadP Draws
drawsP = do
  numbers <- sepBy1 integerP (char ',')
  eol
  return numbers

-- Only returns a result if the parser completely consumes the input
complete :: (Show a) => ReadP a -> String -> a
complete parser input =
  case readP_to_S parser input of
    [(result, "")] -> result
    result -> error $ "Error, incomplete or ambiguous parse: " ++ show result

boardP :: ReadP Board
boardP = do
  rows <- count 5 rowP
  eol <++ eof
  return rows
  where
    rowP = do
      optional whitespace
      row <- sepBy1 integerP whitespace
      eol
      return row

inputP :: ReadP (Draws, [Board])
inputP = do
  draws <- drawsP
  eol
  boards <- many1 boardP
  eof
  return (draws, boards)
