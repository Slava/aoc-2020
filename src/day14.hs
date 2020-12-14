import Data.List (elemIndices, isPrefixOf)
import qualified Data.Map as M
import Data.Bits

-- mask, memory
type State = ((Int, Int), M.Map Int Int)

parseMask :: String -> (Int, Int)
parseMask str = (setMask, clearMask)
  where ones = elemIndices '1' $ reverse str
        zeros = elemIndices '0' $ reverse str
        setMask = foldl setBit 0 ones
        clearMask = complement $ foldl setBit 0 zeros

executeCommand :: State -> String -> State
executeCommand (mask, mem) cmd
  | addr == "mask" = ((parseMask val), mem)
  | "mem" `isPrefixOf` addr = (mask, M.insert memAddr (applyMask (read val)) mem)
  where [addr, _, val] = words cmd
        t = drop 4 addr
        memAddr = read $ take (length t - 1) t
        applyMask x = (x .|. (fst mask)) .&. (snd mask)

process :: [String] -> Int
process cmds = ans
  where (_, mem) = foldl executeCommand ((0, 0), M.empty) cmds
        ans = sum mem

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
