import Data.List.Extra (splitOn)
import qualified Data.Map as M

play :: (Int, Int, M.Map Int Int) -> (Int, Int, M.Map Int Int)
play (n, last, mem) = case M.lookup last mem of
  Nothing -> (n + 1, 0, updatedMem)
  Just pos -> (n + 1, n - pos - 1, updatedMem)
  where updatedMem = M.insert last (n - 1) mem

process :: [String] -> Int
process ls = allNumbers !! (30000000 - 1)
  where xs = map read ls
        xsInit = init xs
        mem = M.fromList $ zip xsInit [0..]
        otherNumbers = map (\(_, num, _) -> num) $ iterate play (length xs, last xs, mem)
        allNumbers = xsInit ++ otherNumbers

main :: IO ()
main = do
  input <- getContents
  print $ process $ splitOn "," input
