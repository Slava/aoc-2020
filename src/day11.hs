import qualified Data.Map as M

data SeatState = Empty | Taken | Floor deriving (Eq, Show)
type State = [((Int, Int), SeatState)]

findFix :: Eq a => (a -> a) -> a -> a
findFix f x =
  if x == nx then x else findFix f nx
  where nx = f x

toState :: [String] -> State
toState ls =
  [((x, y), toSeatState (ls !! x !! y)) | x <- [0..lx-1], y <- [0..ly-1]]
  where toSeatState 'L' = Empty
        toSeatState _ = Floor
        lx = length ls
        ly = length $ head ls

evolve :: State -> State
evolve st =
  map evolveCell st
  where neighbors x y = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
        countLabel label x y = length $ filter (==label) [M.findWithDefault Empty coor stMap | coor <- neighbors x y]
        evolveCellLabel (x, y) Empty = if countLabel Taken x y == 0 then Taken else Empty
        evolveCellLabel (x, y) Taken = if countLabel Taken x y >= 4 then Empty else Taken
        evolveCellLabel _ l = l
        evolveCell (coor, l) = (coor, evolveCellLabel coor l)
        stMap = M.fromList st

process :: [String] -> Int
process ls = length $ filter (\(_, l) -> l == Taken) fixedPoint
  where initState = toState ls
        fixedPoint = findFix evolve initState

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
