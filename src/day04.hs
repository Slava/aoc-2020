import Data.List.Split
import qualified Data.Map as M

type Passport = M.Map String String

parsePassport :: String -> Passport
parsePassport str =
  M.fromList $ map ((\(key:value:_) -> (key, value)) . (splitOn ":")) parts
  where parts = words str

isValid :: Passport -> Bool
isValid passport = all (\field -> M.member field passport) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

process :: [String] -> Int
process allData =
  length valids
  where passports = map parsePassport allData
        valids = filter isValid passports

main :: IO ()
main = do
  input <- getContents
  print $ process $ splitOn "\n\n" input
