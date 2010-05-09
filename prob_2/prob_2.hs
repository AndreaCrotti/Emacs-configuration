module Main where

main = do
  print $ sum [ x | x <- takeWhile (<= 4 * 1000 * 1000) fibs,
                         even x]
      where
        fibs = 1 : 1 : zipWith (+) fibs (tail fibs)