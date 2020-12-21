import Data.List.Extra (splitOn)
import qualified Data.Map as M

data Rule = Literal Char | Choices [[String]]

parseRule :: String -> (String, Rule)
parseRule str = case parts of
  ['"', literal, '"'] -> (name, Literal literal)
  _ -> (name, Choices choices)
  where [name, parts] = splitOn ": " str
        choices = map words $ splitOn " | " parts

process :: String -> Int
process input = length [l | l <- ls, parses l]
  where [grammar, inp] = splitOn "\n\n" input
        rules = M.fromList $ map parseRule $ lines grammar
        ls = lines inp

        parses :: String -> Bool
        parses = match (getSubrules ["0"])

        getSubrules :: [String] -> [Rule]
        getSubrules = map (rules M.!)

        -- stack of rules
        match :: [Rule] -> String -> Bool
        match [] [] = True
        match [] _ = False
        match _ [] = False
        match (Literal c:stack) (x:xs) = c == x && match stack xs
        match (Choices cs:stack) xs = any ((\moreRules -> match (moreRules ++ stack) xs) . getSubrules) cs

main :: IO ()
main = do
  input <- getContents
  print $ process input
