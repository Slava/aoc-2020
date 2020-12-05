checkSlope :: Int -> Int -> [String] -> Int
checkSlope dx dy legend =
  length $ filter (\(x, y) -> legend !! y !! x == '#') positions
  where height = length legend
        width = length $ head legend
        positions = [((dx * i) `mod` width, dy * i) | i <- [0..(height `div` dy)-1]]

process :: [String] -> Int
process legend =
  product $ map (\(x, y) -> checkSlope x y legend) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
