import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes)

data Instr = Noop Int | Acc Int | Jmp Int

parseInstr :: String -> Instr
parseInstr str =
  case ins of
    "acc" -> Acc num
    "jmp" -> Jmp num
    _ -> Noop num
  where ins:numStr:_ = words str
        num = read $ filter (/='+') numStr

evaluate :: Instr -> (Int, Int)
evaluate (Noop _) = (0, 1)
evaluate (Acc acc) = (acc, 1)
evaluate (Jmp jmp) = (0, jmp)

repair :: Instr -> Instr
repair (Noop x) = Jmp x
repair (Jmp x) = Noop x
repair x = x

execute :: M.Map Int Instr -> S.Set Int -> Int -> Maybe Int
execute program visited pos
  | jmp `M.notMember` program = Just 0
  | jmp `S.member` visited = Nothing
  | otherwise = sum <$> sequence [Just acc, execute program (S.insert jmp visited) jmp]
  where instr = program M.! pos
        (acc, adv) = evaluate instr
        jmp = pos + adv

process :: [String] -> Int
process ls =
  head successes
  where instrs = map parseInstr ls
        program = M.fromList $ zip [0..] instrs
        tryRepair idx instr = execute (M.insert idx (repair instr) program) (S.singleton 0) 0
        attempts = [tryRepair idx instr | (idx, instr) <- M.toList program]
        successes = catMaybes attempts

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
