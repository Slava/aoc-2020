data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show)

turnRight :: Direction -> Direction
turnRight d
  | d == maxBound = minBound
  | otherwise = succ d

turnLeft :: Direction -> Direction
turnLeft d
  | d == minBound = maxBound
  | otherwise = pred d

type ShipState = ((Int, Int), Direction)

applyAction :: String -> ShipState -> ShipState
applyAction (i:num) ((x, y), dir) = case i of
  'F' -> move dir
  'N' -> move North
  'S' -> move South
  'E' -> move East
  'W' -> move West
  'R' -> ((x, y), iterate turnRight dir !! (amt `div` 90))
  'L' -> ((x, y), iterate turnLeft dir !! (amt `div` 90))
  where amt = read num
        move North = ((x, y + amt), dir)
        move South = ((x, y - amt), dir)
        move East = ((x + amt, y), dir)
        move West = ((x - amt, y), dir)

process :: [String] -> Int
process instructions = abs x + abs y
  where ((x, y), _) = foldr applyAction ((0, 0), East) instructions

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
