import Data.List (transpose)
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

rotateImage :: [[a]] -> [[a]]
rotateImage = transpose . reverse

rotateImageN :: Int -> [[a]] -> [[a]]
rotateImageN n im = iterate rotateImage im !! n

rotate :: Tile -> Tile
rotate Tile {name=name, rows=rows} = Tile {name=name, rows=rotateImage rows }

rotateN :: Int -> Tile -> Tile
rotateN n tile = iterate rotate tile !! n

data Axis = ANone | AX | AY | AXY

flipImage :: Axis -> [[a]] -> [[a]]
flipImage AX = map reverse
flipImage AY = reverse
flipImage AXY = flipImage AX . flipImage AY
flipImage ANone = id

flipTile :: Axis -> Tile -> Tile
flipTile axis Tile{name=name, rows=rows} = Tile{name=name, rows=flipImage axis rows}

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

getPuzzleImage :: PuzzleState -> [String]
getPuzzleImage st = ls
  where n = floor . sqrt . fromIntegral $ length $ M.toList st
        k = length $ rows $ snd $ head $ M.toList st

        showLine :: Int -> Int -> String
        showLine row line = concat [rows tile !! line | column <- [0..n-1], let tile = st M.! (column, row)]

        showRow :: Int -> [String]
        showRow row = map (showLine row) [0..k-1]

        ls = concatMap showRow [0..n-1]

solvePuzzle :: String -> Maybe PuzzleState
solvePuzzle input = solution
  where tiles :: [Tile]
        tiles = map read $ splitOn "\n\n" input
        solution = getFirst $ fillPuzzle tiles (0, 0) M.empty S.empty

trimBorders :: [String] -> [String]
trimBorders ls = trim $ map trim ls
  where trim = init . tail

trimPuzzle :: PuzzleState -> PuzzleState
trimPuzzle puz = M.fromList [(pos, trimTile tile) | (pos, tile) <- M.toList puz]
  where trimTile Tile{name=name, rows=rows} = Tile{name=name, rows=trimBorders rows}

seaMonster :: [String]
seaMonster =
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "]

seaMonsterPos :: [Pos]
seaMonsterPos = [(x, y) | (y, row) <- zip [0..] seaMonster, (x, c) <- zip [0..] row, c == '#']

offset :: Pos -> [Pos] -> [Pos]
offset (x, y) ps = [(x + sx, y + sy) | (sx, sy) <- ps]

imageSet :: [String] -> S.Set Pos
imageSet im = S.fromList [(x, y) | (y, row) <- zip [0..] im, (x, c) <- zip [0..] row, c == '#']

findSeaMonsters :: [String] -> (Int, Axis) -> S.Set Pos
findSeaMonsters im (rotations, flips) = S.unions $ map (S.fromList . untransform) foundMonsters
  where isSeaMonster :: [Pos] -> Bool
        isSeaMonster = all (`S.member` imSet)

        transformedIm = rotateImageN rotations . flipImage flips $ im
        imSet = imageSet transformedIm
        pos = [[(x, y) | (x, _) <- zip [0..] row] | (y, row) <- zip [0..] im]
        transformedPos = rotateImageN rotations . flipImage flips $ pos
        untransformMapping = M.fromList [((x, y), originalPos) | (y, row) <- zip [0..] transformedPos, (x, originalPos) <- zip [0..] row]

        untransform :: [Pos] -> [Pos]
        untransform = map (untransformMapping M.!)

        foundMonsters = [ adjMonsterPos
                        | (y, row) <- zip [0..] transformedIm
                        , (x, _) <- zip [0..] row
                        , let adjMonsterPos = offset (x, y) seaMonsterPos
                        , isSeaMonster adjMonsterPos]

findAllSeaMonsters :: [String] -> S.Set Pos
findAllSeaMonsters im = S.unions [findSeaMonsters im tr | tr <- uniqueTransforms]

solveSeaMonsters :: PuzzleState -> Int
solveSeaMonsters puzzle = length $ S.toList (imageSet im S.\\ monsters)
  where im = getPuzzleImage . trimPuzzle $ puzzle
        monsters = findAllSeaMonsters im

main :: IO ()
main = do
  input <- getContents
  let maybeSolution = solvePuzzle input in
    case maybeSolution of
      Just solution -> print (solveSeaMonsters solution)
      Nothing -> putStrLn "did not find a solution"
