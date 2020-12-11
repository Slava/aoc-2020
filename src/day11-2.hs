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
  where directions = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
        checkDirection (dx, dy) x y = case M.lookup (x + dx, y + dy) stMap of
          Nothing -> False
          Just Taken -> True
          Just Empty -> False
          Just Floor -> checkDirection (dx, dy) (x + dx) (y + dy)
        countTaken x y = length $ filter (==True) [checkDirection dir x y | dir <- directions]
        evolveCellLabel (x, y) Empty = if countTaken x y == 0 then Taken else Empty
        evolveCellLabel (x, y) Taken = if countTaken x y >= 5 then Empty else Taken
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
