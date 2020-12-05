import Data.List.Split
import qualified Data.Map as M
import Text.Regex.TDFA

type Passport = M.Map String String

parsePassport :: String -> Passport
parsePassport str =
  M.fromList $ map ((\(key:value:_) -> (key, value)) . (splitOn ":")) parts
  where parts = words str

validInt :: Int -> Int -> String -> Bool
validInt mn mx str = str =~ "^[0-9]{4}$" && (read str) >= mn && (read str) <= mx

validHeight :: String -> Bool
validHeight str =
  validFormat && num >= mn && num <= mx
  where validFormat = str =~ "^[0-9]+(cm|in)$"
        unit = if validFormat then drop ((length str) - 2) str else "cm"
        num = (read $ take ((length str) - 2) str)::Int
        mn = if unit == "cm" then 150 else 59
        mx = if unit == "cm" then 193 else 76

validHcl :: String -> Bool
validHcl str = str =~ "^#[0-9a-f]{6}$"

validEcl :: String -> Bool
validEcl str = str `elem` (words "amb blu brn gry grn hzl oth")

validPid :: String -> Bool
validPid str = str =~ "^[0-9]{9}$"

isValid :: Passport -> Bool
isValid passport =
  all validate [
  ("byr", validInt 1920 2002),
  ("iyr", validInt 2010 2020),
  ("eyr", validInt 2020 2030),
  ("hgt", validHeight),
  ("hcl", validHcl),
  ("ecl", validEcl),
  ("pid", validPid)]
  where validate (field, validator) = maybe False validator $ M.lookup field passport

process :: [String] -> Int
process allData =
  length valids
  where passports = map parsePassport allData
        valids = filter isValid passports

main :: IO ()
main = do
  input <- getContents
  print $ process $ splitOn "\n\n" input
