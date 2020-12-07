import Data.List.Extra (isInfixOf, splitOn)

parseRule :: String -> [(String, String, Int)]
parseRule rule =
  map makeRule childrenParsed
  where parts = words rule
        parent:children:_ = splitOn ["contain"] parts
        parseNum expr = ((unwords . tail . words) expr, (read . head . words) expr)
        childrenParsed = map parseNum $ splitOn ", " $ unwords children
        normalizeName name = unwords $ filter (`notElem` ["bag", "bags", "bag.", "bags."]) $ words name
        makeRule (child, num) = ((normalizeName . unwords) parent, normalizeName child, num)

relevantRule :: String -> Bool
relevantRule rule = not $ "contain no other bags" `isInfixOf` rule

countSubtree :: [(String, String, Int)] -> String -> Int
countSubtree rules t =
  1 + others
  where ts = [(to, w) | (from, to, w) <- rules, from == t]
        branch (to, w) = w * countSubtree rules to
        others = sum $ map branch ts

process :: [String] -> Int
process rules =
  (countSubtree parsedRules "shiny gold") - 1
  where filteredRules = filter relevantRule rules
        parsedRules = concatMap parseRule filteredRules

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
