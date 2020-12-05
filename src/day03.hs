process :: Int -> Int -> [String] -> Int
process dx dy legend =
  length $ filter (\(x, y) -> legend !! y !! x == '#') positions
  where height = length legend
        width = length $ head legend
        positions = [((dx * i) `mod` width, dy * i) | i <- [0..(height `div` dy)-1]]

main :: IO ()
main = do
  input <- getContents
  print $ process 3 1 $ lines input
