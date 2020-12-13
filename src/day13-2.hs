import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (abs a, signum a, 0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g,x,y) = egcd b (a `mod` b)

inverse :: Integer -> Integer -> Integer
inverse m a = y `mod` m
  where
    (_,_,y) = egcd m a

solveMod :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
solveMod a b m
  | g == 1         = Just ((b * inverse m a) `mod` m, m)
  | b `mod` g == 0 = solveMod (a `div` g) (b `div` g) (m `div` g)
  | otherwise      = Nothing
  where
    g = gcd a m

gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a,n) (b,m)
  | a `mod` g == b `mod` g = Just (((a*v*m + b*u*n) `div` g) `mod` k, k)
  | otherwise              = Nothing
  where
    (g,u,v) = egcd n m
    k = (m*n) `div` g

gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt []         = Nothing
gcrt [e]        = Just e
gcrt (e1:e2:es) = gcrt2 e1 e2 >>= \e -> gcrt (e:es)

process :: [String] -> Integer
process (_:str2:_) = earliest
  where offsetBus = zip [0..] $ splitOn "," str2
        reqs = [(read busStr, offset) | (offset, busStr) <- offsetBus, busStr /= "x"]
        mods = map fst reqs
        remainders = [((period - offset) `mod` period + period) `mod` period | (period, offset) <- reqs]
        (earliest, _) = fromMaybe (0, 0) $ gcrt $ zip remainders mods

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
