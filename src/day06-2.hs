import Data.List.Split (splitOn)
import Data.List (intersect)

process :: [[String]] -> Int
process groups =
  sum $ map countGroup groups
  where countGroup group = length $ foldr1 intersect  group

main :: IO ()
main = do
  input <- getContents
  print $ process $ map lines $ splitOn "\n\n" input
