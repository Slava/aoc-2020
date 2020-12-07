import Data.List.Extra (nubOrd, isInfixOf, splitOn)

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Int, String)] of
      [(_, "")] -> True
      _         -> False

parseRule :: String -> [(String, String)]
parseRule rule =
  map makeRule childrenParsed
  where relevantPart = not . isNumber
        parts = filter relevantPart $ words rule
        parent:children:_ = splitOn ["contain"] parts
        childrenParsed = splitOn ", " $ unwords children
        normalizeName name = unwords $ filter (`notElem` ["bag", "bags", "bag.", "bags."]) $ words name
        makeRule child = ((normalizeName . unwords) parent, normalizeName child)

relevantRule :: String -> Bool
relevantRule rule = not $ "contain no other bags" `isInfixOf` rule

reachable :: [(String, String)] -> String -> [String]
reachable rules t =
  ts ++ others
  where ts = [from | (from, to) <- rules, to == t]
        others = concatMap (reachable rules) ts

process :: [String] -> Int
process rules =
  length $ nubOrd $ reachable parsedRules "shiny gold"
  where filteredRules = filter relevantRule rules
        parsedRules = concatMap parseRule filteredRules

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
