{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forever)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Exit (exitSuccess)
import System.IO (isEOF)

main :: IO ()
main =
  forever $
    isEOF >>= \case
      True -> exitSuccess
      False -> getLine >>= processSecret
  where
    processSecret :: String -> IO ()
    processSecret secret = do
      putStrLn $ "Part 1: " ++ show (part1 secret)
      putStrLn $ "Part 2: " ++ show (part2 secret)

part1 :: String -> Int
part1 = (fst . fromJust . find (startsWith5Zeros . snd)) . enumerateMD5

part2 :: String -> Int
part2 = (fst . fromJust . find (startsWith6Zeros . snd)) . enumerateMD5

md5Word8 :: String -> [Word8]
md5Word8 = B.unpack . MD5.hash . BU.fromString

startsWith5Zeros :: [Word8] -> Bool
startsWith5Zeros (b0 : b1 : b2 : _) = b0 == 0 && b1 == 0 && b2 <= 0x0f
startsWith5Zeros _ = False

startsWith6Zeros :: [Word8] -> Bool
startsWith6Zeros (b0 : b1 : b2 : _) = b0 == 0 && b1 == 0 && b2 == 0
startsWith6Zeros _ = False

enumerateMD5 :: String -> [(Int, [Word8])]
enumerateMD5 secret = map (\i -> (i, md5Word8 $ secret ++ show i)) [1 ..]
