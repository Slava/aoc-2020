import Data.List.Extra (splitOn, minimumOn)

process :: [String] -> Int
process (str1:str2:_) = waitTime * busId
  where depTime = read str1
        buses = map read $ filter (/="x") $ splitOn "," str2
        earliest busId = (depTime + busId - 1) `div` busId * busId
        busId = minimumOn earliest buses
        waitTime = earliest busId - depTime

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
