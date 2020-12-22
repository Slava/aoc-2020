import Data.List.Extra (splitOn)
import qualified Data.Set as S

data Player = A | B deriving Eq

play :: S.Set ([Int], [Int]) -> [Int] -> [Int] -> (Player, [Int])
play _ [] bs = (B, bs)
play _ as [] = (A, as)
play history (a:as) (b:bs)
  | S.member (nextAs, nextBs) history = (A, nextAs)
  | otherwise = play nextHistory nextAs nextBs
  where canSubgame = length as >= a && length bs >= b
        (subgameWinner, _) = play S.empty (take a as) (take b bs)
        turnWinner
          | canSubgame = subgameWinner
          | a > b = A
          | otherwise = B
        (nextAs, nextBs) = if turnWinner == A then (as++[a, b], bs) else (as, bs++[b, a])
        nextHistory = S.insert (a:as, b:bs) history

score :: [Int] -> Int
score xs = sum $ zipWith (*) (reverse xs) [1..]

process :: String -> Int
process input = score $ snd $ play S.empty as bs
  where [astr, bstr] = splitOn "\n\n" input
        as = map read $ tail $ lines astr
        bs = map read $ tail $ lines bstr

main :: IO ()
main = do
  input <- getContents
  print $ process input
