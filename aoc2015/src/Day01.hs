module Main (main) where

main :: IO ()
main = do
  input <- getContents
  let ups = length $ filter ('(' ==) input
  let downs = length $ filter (')' ==) input
  let firstBasementEntry =
        ( length
            . takeWhile (/= -1)
            . scanl (+) 0
            . map charToInt
            . filter isValidChar
        )
          input
  putStrLn $ "Part 1: " ++ show (ups - downs)
  putStrLn $ "Part 2: " ++ show firstBasementEntry
  where
    isValidChar :: Char -> Bool
    isValidChar c = c `elem` ['(', ')']

    charToInt :: Char -> Int
    charToInt c = case c of
      '(' -> 1
      ')' -> -1
      _ -> undefined
