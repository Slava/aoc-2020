import Data.List.Split

isValid :: [Int] -> Char -> String -> Bool
isValid idxs c password =
  occurences == 1
  where occurences = length $ filter (c==) [password !! (idx - 1) | idx <- idxs]

parseAndValidate :: String -> Bool
parseAndValidate input =
  isValid idxs (head criteria) password
  where range:criteria:password:_ = splitOn " " input
        idxs = map read $ splitOn "-" range

process :: [String] -> Int
process list = length $ filter parseAndValidate list

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
