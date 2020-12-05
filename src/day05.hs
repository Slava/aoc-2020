import Data.List (elemIndex)
import Data.Maybe (listToMaybe, fromMaybe)
import Numeric    (readInt)

readPatBin :: String -> Char -> Int
readPatBin pat = (fromMaybe 0) . (`elemIndex` pat)

readBin :: Integral a => String -> String -> Maybe a
readBin pat = fmap fst . listToMaybe . readInt 2 (`elem` pat) (readPatBin pat)

process :: [String] -> [Int]
process strs = map toId strs
  where toId str = (fromMaybe 0 (readBin "FB" str)) * 8 + (fromMaybe 0 (readBin "LR" (drop 7 str)))

main :: IO ()
main = do
  input <- getContents
  print $ maximum $ process $ lines input
