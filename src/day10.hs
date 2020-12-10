import Data.List (sort)

count :: [Int] -> (Int, Int)
count [_] = (0, 1)
count (x:y:rest) =
  case y-x of
    1 -> (ones + 1, threes)
    3 -> (ones, threes + 1)
    _ -> (ones, threes)
  where (ones, threes) = count (y:rest)
count [] = (0, 0)

process :: [Int] -> Int
process xs = ones * threes
  where ys = sort xs
        (ones, threes) = count (0:ys)


main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ lines input
