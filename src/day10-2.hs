import Data.List (sort)
import Data.Array (listArray, (!))

process :: [Int] -> Int
process xs = nways
  where ys = listArray (0, length xs) $ 0 : sort xs
        len = length ys
        countMemo = (map count [0..] !!)
        count idx
          | idx + 1 == len = 1
          | otherwise = sum [countMemo i | i <- [idx+1..idx+3], i < len && (ys ! i - ys ! idx) <= 3]
        nways = countMemo 0


main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ lines input
