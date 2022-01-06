module Iterables (
  enumerate
) where

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
