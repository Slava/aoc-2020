import Data.List (elemIndex)
import Data.Maybe (listToMaybe, fromMaybe)
import Numeric    (readInt)
import Data.Bits

readPatBin :: String -> Char -> Int
readPatBin pat = fromMaybe 0 . (`elemIndex` pat)

readBin :: Integral a => String -> String -> Maybe a
readBin pat = fmap fst . listToMaybe . readInt 2 (`elem` pat) (readPatBin pat)

readId :: String -> Int
readId str = fromMaybe 0 (readBin "FB" str) * 8 + fromMaybe 0 (readBin "LR" (drop 7 str))

process :: [String] -> Int
process strs = missing
  where ids = map readId strs
        mn = minimum ids
        mx = maximum ids
        xorAll = foldl1 xor [mn..mx]
        missing = xor xorAll $ foldl1 xor ids

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
