
main = do
  print $ sum $ filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) [0..1000]