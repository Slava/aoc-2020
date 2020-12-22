import Data.List.Extra (splitOn, nubOrd, intersect)

parseIngrList :: String -> ([String], [String])
parseIngrList str = (words ingrs, splitOn ", " allergns)
  where [ingrs, allergns] = splitOn "(contains " $ init str

process :: [String] -> Int
process ls = length nonSusIngrs
  where xs = map parseIngrList ls
        allergns = nubOrd $ concatMap snd xs
        susIngrs = nubOrd $ concat [foldr1 intersect [is | (is, as) <- xs, allergn `elem` as] | allergn <- allergns]
        nonSusIngrs = filter (not . (`elem` susIngrs)) $ concatMap fst xs

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
