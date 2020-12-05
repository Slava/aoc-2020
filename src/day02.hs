import Data.List.Split

isValid :: Int -> Int -> Char -> String -> Bool
isValid mn mx c password =
  occurences >= mn && occurences <= mx
  where occurences = length $ filter (c==) password

parseAndValidate :: String -> Bool
parseAndValidate input =
  isValid (read mn) (read mx) (head critera) password
  where range:critera:password:_ = splitOn " " input
        mn:mx:_ = splitOn "-" range

process :: [String] -> Int
process list = length $ filter parseAndValidate list

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
