module Parsing
  ( eol,
    readInteger,
    parseAndThen,
  )
where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    choice,
    many1,
    readP_to_S,
    satisfy,
  )

eol :: ReadP Char
eol = choice (map char ['\t', '\n', '\r', ' '])

readInteger :: ReadP Int
readInteger = do
  digits <- many1 (satisfy isDigit)
  return $ read digits

parseAndThen :: Show a => ReadP a -> String -> (a -> b) -> b
parseAndThen parser input mapper =
  case readP_to_S parser input of
    [(result, "")] -> mapper result
    [] -> error $ "Parse error: (invalid input): " ++ show input
    rs -> error $ "Parse error: (ambiguous parse): " ++ show rs
