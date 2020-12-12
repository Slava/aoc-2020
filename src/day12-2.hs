data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (x, y) = (y, -x)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (x, y) = (-y, x)

type ShipState = ((Int, Int), (Int, Int))

applyAction :: ShipState -> String -> ShipState
applyAction ((x, y), (sx, sy)) (i:num) = case i of
  'F' -> ((x, y), (sx + x * amt, sy + y * amt))
  'N' -> move North
  'S' -> move South
  'E' -> move East
  'W' -> move West
  'R' -> (iterate turnRight (x, y) !! (amt `div` 90), (sx, sy))
  'L' -> (iterate turnLeft (x, y) !! (amt `div` 90), (sx, sy))
  where amt = read num
        move North = ((x, y + amt), (sx, sy))
        move South = ((x, y - amt), (sx, sy))
        move East = ((x + amt, y), (sx, sy))
        move West = ((x - amt, y), (sx, sy))

process :: [String] -> Int
process instructions = abs sx + abs sy
  where ((x, y), (sx, sy)) = foldl applyAction ((10, 1), (0, 0)) instructions

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
