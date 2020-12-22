import Data.List.Extra (splitOn, nubOrd, intersect, sort, intercalate, permutations)
import Debug.Trace

parseIngrList :: String -> ([String], [String])
parseIngrList str = (words ingrs, splitOn ", " allergns)
  where [ingrs, allergns] = splitOn "(contains " $ init str

process :: [String] -> String
process ls = intercalate "," ans
  where xs = map parseIngrList ls
        allergns = sort $ nubOrd $ concatMap snd xs
        sets = [foldr1 intersect [is | (is, as) <- xs, allergn `elem` as] | allergn <- allergns]
        susIngrs = nubOrd $ concat sets
        ans = head $ filter isCorrect (permutations susIngrs)

        isCorrect ingrs = and [ingr `elem` set | (set, ingr) <- zip sets ingrs]

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
