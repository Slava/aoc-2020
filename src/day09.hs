preambleLength :: Int
preambleLength = 25

findBad :: [Int] -> Int
findBad xs
  | x `elem` combos = findBad $ tail xs
  | otherwise = x
  where preamble = take preambleLength xs
        x:_ = drop preambleLength xs
        combos = [x+y | x <- preamble, y <- preamble, x /= y]

main :: IO ()
main = do
  input <- getContents
  print $ findBad $ map read $ lines input
