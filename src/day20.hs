import Data.List (transpose, intercalate)
import Data.List.Extra (splitOn, nubOrdOn)
import Data.Monoid (getFirst, First(..))
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

data Tile = Tile { name :: Int, rows :: [String] } deriving Show

instance Read Tile where
  readsPrec _ input = [(Tile { name = name, rows = rows }, "")]
    where (header:ls) = lines input
          name = read $ init $ last $ words header
          rows = ls

data Side = STop | SBottom | SLeft | SRight

rotate :: Tile -> Tile
rotate Tile {name=name, rows=rows} = Tile {name=name, rows=rotateRows rows }
  where rotateRows = transpose . reverse

rotateN :: Int -> Tile -> Tile
rotateN n tile = iterate rotate tile !! n

data Axis = ANone | AX | AY | AXY

flipTile :: Axis -> Tile -> Tile
flipTile AX Tile{name=name, rows=rows} = Tile{name=name, rows=map reverse rows}
flipTile AY Tile{name=name, rows=rows} = Tile{name=name, rows=reverse rows}
flipTile AXY tile = (flipTile AX . flipTile AY) tile
flipTile ANone tile = tile

getSide :: Side -> Tile -> String
getSide STop = head . rows
getSide SBottom = last . rows
getSide SLeft = getSide STop . rotate
getSide SRight = getSide SBottom . rotate

type Pos = (Int, Int)
type PuzzleState = M.Map Pos Tile
type UsedTiles = S.Set Int

uniqueTransforms :: [(Int, Axis)]
uniqueTransforms = nubOrdOn appliedOnExample [(rotations, flips) | rotations <- [0..3], flips <- [AX, AY, AXY, ANone]]
  where appliedOnExample (rotations, flips) = rows $ rotateN rotations . flipTile flips $ tile
        tile = Tile{name=0, rows=["012", "345", "678"]}

fillPuzzle :: [Tile] -> (Int, Int) -> PuzzleState -> UsedTiles -> First PuzzleState
fillPuzzle tiles (x, y) st used = if y == n then First (Just st) else firstSolution
  where branches :: [First PuzzleState]
        branches = [ fillPuzzle tiles (nextX, nextY) (M.insert (x, y) transformedTile st) (S.insert (name tile) used)
                   | tile <- tiles
                   , name tile `S.notMember` used
                   -- , rotations <- [0..3]
                   -- , flips <- [AX, AY, AXY, ANone]
                   , (rotations, flips) <- uniqueTransforms
                   , let transformedTile = rotateN rotations . flipTile flips $ tile
                   , matches transformedTile
                   ]

        matches :: Tile -> Bool
        matches tile = maybeMatch topNeighborSide topSide && maybeMatch leftNeighborSide leftSide
          where leftSide = getSide SLeft tile
                topSide = getSide STop tile

        leftNeighborSide = getNeighborSide SRight (x - 1, y)
        topNeighborSide = getNeighborSide SBottom (x, y - 1)

        getNeighborSide :: Side -> Pos -> Maybe String
        getNeighborSide side pos = getSide side <$> M.lookup pos st

        maybeMatch :: Maybe String -> String -> Bool
        maybeMatch Nothing _ = True
        maybeMatch (Just x) y = x == y

        firstSolution = foldl (<>) (First Nothing) branches

        n = floor . sqrt . fromIntegral $ length tiles
        (nextX, nextY) = if x == n-1 then (0, y + 1) else (x + 1, y)

-- debug print
showPuzzle :: PuzzleState -> String
showPuzzle st = unlines ls
  where n = floor . sqrt . fromIntegral $ length $ M.toList st
        k = length $ rows $ snd $ head $ M.toList st

        showLine :: Int -> Int -> String
        showLine row line = unwords [rows tile !! line | column <- [0..n-1], let tile = st M.! (column, row)]

        showRow :: Int -> [String]
        showRow row = map (showLine row) [0..k-1]

        ls = intercalate [""] $ map showRow [0..n-1]


process :: String -> Maybe PuzzleState
process input = solution
  where tiles :: [Tile]
        tiles = map read $ splitOn "\n\n" input
        solution = getFirst $ fillPuzzle tiles (0, 0) M.empty S.empty

outputAns :: PuzzleState -> Int
outputAns puzzle = product $ map name cornerTiles
  where n = floor . sqrt . fromIntegral $ length $ M.toList puzzle
        cornerTiles = map (puzzle M.!) [(0, 0), (0, n-1), (n-1, 0), (n-1, n-1)]

main :: IO ()
main = do
  input <- getContents
  let maybeSolution = process input in
    case maybeSolution of
      Just solution -> putStrLn (showPuzzle solution) >> print (outputAns solution)
      Nothing -> putStrLn "did not find a solution"
