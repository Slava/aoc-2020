import Data.List (elemIndices, isPrefixOf)
import qualified Data.Map as M
import Data.Bits

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- mask, memory
type State = ((Int, [Int]), M.Map Int Int)

parseMask :: String -> (Int, [Int])
parseMask str = (setMask, xs)
  where ones = elemIndices '1' $ reverse str
        xs = elemIndices 'X' $ reverse str
        setMask = foldl setBit 0 ones

generateAddrs :: (Int, [Int]) -> Int -> [Int]
generateAddrs (setMask, floatingIdxs) addr =
  [(addr .|. setMask .|. (setBits bits)) .&. (clearBits bits) | bits <- subsets floatingIdxs]
  where setBits subset = foldl setBit 0 subset
        clearBits subset = complement $ foldl setBit 0 $ filter (`notElem` subset) floatingIdxs

executeCommand :: State -> String -> State
executeCommand (mask, mem) cmd
  | addr == "mask" = ((parseMask val), mem)
  | "mem" `isPrefixOf` addr = (mask, updatedMem)
  where [addr, _, val] = words cmd
        t = drop 4 addr
        memAddr = read $ take (length t - 1) t
        readVal = read val
        setValue m dest = M.insert dest readVal m
        updatedMem = foldl setValue mem $ generateAddrs mask memAddr

process :: [String] -> Int
process cmds = ans
  where (_, mem) = foldl executeCommand ((0, []), M.empty) cmds
        ans = sum mem

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
