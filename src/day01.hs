process :: [Int] -> Int
process list = head [x * y | x <- list, y <- list, x + y == 2020]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ lines input
