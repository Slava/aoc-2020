import Data.List.Split (splitOn)
import Data.List.Extra (nubOrd)

process :: [[String]] -> Int
process groups =
  sum $ map countGroup groups
  where countGroup group = length $ nubOrd $ concat group

main :: IO ()
main = do
  input <- getContents
  print $ process $ map lines $ splitOn "\n\n" input
