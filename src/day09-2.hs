preambleLength :: Int
preambleLength = 25

findBad :: [Int] -> Int
findBad xs
  | x `elem` combos = findBad $ tail xs
  | otherwise = x
  where preamble = take preambleLength xs
        x:_ = drop preambleLength xs
        combos = [x+y | x <- preamble, y <- preamble, x /= y]

findContig :: [Int] -> Int -> Int
findContig xs bad =
  minimum answer + maximum answer
  where maxIdx = length xs - 1
        sub start end = drop start $ take (end + 1) xs
        subSum start end = sum $ sub start end
        answer = head [sub start end | start <- [0..maxIdx], end <- [start+1..maxIdx], subSum start end == bad]

main :: IO ()
main = do
  input <- getContents
  let xs = map read $ lines input
  print $ findContig xs $ findBad xs
