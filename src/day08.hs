import qualified Data.Map as M
import qualified Data.Set as S

data Instr = Noop | Acc Int | Jmp Int

parseInstr :: String -> Instr
parseInstr str =
  case ins of
    "acc" -> Acc num
    "jmp" -> Jmp num
    _ -> Noop
  where ins:numStr:_ = words str
        num = read $ filter (/='+') numStr

evaluate :: Instr -> (Int, Int)
evaluate Noop = (0, 1)
evaluate (Acc acc) = (acc, 1)
evaluate (Jmp jmp) = (0, jmp)

execute :: M.Map Int Instr -> S.Set Int -> Int -> Int
execute program visited pos =
  if jmp `S.member` visited
  then 0
  else acc + execute program (S.insert jmp visited) jmp
  where instr = program M.! pos
        (acc, adv) = evaluate instr
        jmp = pos + adv

process :: [String] -> Int
process ls =
  execute program (S.singleton 0) 0
  where instrs = map parseInstr ls
        program = M.fromList $ zip [0..] instrs

main :: IO ()
main = do
  input <- getContents
  print $ process $ lines input
