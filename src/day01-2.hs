process :: [Int] -> Int
process list = head [x * y * z | x <- list, y <- list, z <- list, x + y + z == 2020]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map read $ lines input
