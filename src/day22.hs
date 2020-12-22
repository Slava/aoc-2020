import Data.List.Extra (splitOn)

play :: [Int] -> [Int] -> [Int]
play [] bs = bs
play (a:as) (b:bs) = if a > b then play bs (as++[a, b]) else play as (bs++[b, a])

score :: [Int] -> Int
score xs = sum $ zipWith (*) (reverse xs) [1..]

process :: String -> Int
process input = score $ play as bs
  where [astr, bstr] = splitOn "\n\n" input
        as = map read $ tail $ lines astr
        bs = map read $ tail $ lines bstr

main :: IO ()
main = do
  input <- getContents
  print $ process input
