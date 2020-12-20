import qualified Data.Map as M

type State = M.Map (Int, Int, Int) Bool
type NState = M.Map (Int, Int, Int) Int -- count of neighbors

fromString :: String -> State
fromString input = M.fromList [((x, y, 0), c == '#') | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l]
  where ls = lines input

combine :: NState -> NState -> NState
combine = M.unionWith (+)

transition :: State -> State
transition st = M.fromList [(pos, True) | (pos, litLevel) <- M.toList litness, staysLit pos litLevel]
  where litness = foldr combine M.empty [litNeighbors pos | (pos, lit) <- M.toList st, lit]
        litNeighbors (x, y, z) = M.fromList [((x + dx, y + dy, z + dz), 1) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dx /= 0 || dy /= 0 || dz /= 0]
        staysLit pos level
          | lookupSt pos && level `elem` [2, 3] = True
          | not (lookupSt pos) && level == 3 = True
          | otherwise = False
        lookupSt pos = M.findWithDefault False pos st


process :: State -> Int
process initSt = countLit $ iterate transition initSt !! 6
  where countLit = length . M.elems

main :: IO ()
main = do
  input <- getContents
  print $ process (fromString input)
