import Data.List.Extra (splitOn)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Set as S

type Rule = (String, [(Int, Int)])
type Ticket = [Int]

parseRule :: String -> Rule
parseRule str = (name, rule)
  where (name:conds:_) = splitOn ": " str
        condParts = filter (/= "or") $ words conds
        parseCond condStr = (from, to)
          where (from:to:_) = map read $ splitOn "-" condStr
        rule = map parseCond condParts

passesRule :: Int -> Rule -> Bool
passesRule x (_, conds) = or [x >= from && x <= to | (from, to) <- conds]

process :: [[String]] -> Int
process (rulesStr:yourTicketStr:nearbyTicketsStr:_) = ans
  where yourTicket = parseTicket $ head $ tail yourTicketStr
        nearbyTickets = map parseTicket $ tail nearbyTicketsStr
        fieldCount = length yourTicket

        parseTicket :: String -> Ticket
        parseTicket str = map read $ splitOn "," str

        isValid :: Ticket -> Bool
        isValid = all (\field -> any (passesRule field) rules)

        rules = map parseRule rulesStr
        validTickets = filter isValid nearbyTickets

        ruleCandidates :: Rule -> [Int]
        ruleCandidates rule = filter (\pos -> all (\ticket -> passesRule (ticket !! pos) rule) validTickets) [0..fieldCount-1]

        -- Optimization 1: precompute candidates
        rulesWithCandidates = [(rule, ruleCandidates rule) | rule <- rules]

        -- Optimization 2: consider rules with lower branching factor first
        orderedRuleCandidates = sortOn (length . snd) rulesWithCandidates

        ruleAssignments :: [(Rule, [Int])] -> S.Set Int -> [[(Rule, Int)]]
        ruleAssignments [] _ = [[]]
        ruleAssignments ((rule, candidates):otherRules) assignedFields =
          concatMap assignField [field | field <- candidates, S.notMember field assignedFields]
          where assignField field = map (prependField field) $ ruleAssignments otherRules (S.insert field assignedFields)
                prependField field assignment = (rule, field):assignment

        correctAssignment = head (ruleAssignments orderedRuleCandidates S.empty)
        ans = product [yourTicket !! fieldPos | ((name, _), fieldPos) <- correctAssignment , "departure" `isPrefixOf` name]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map lines $ splitOn "\n\n" input
