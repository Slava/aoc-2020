import Data.List.Extra (splitOn)

parseRule :: String -> [(Int, Int)]
parseRule str = rule
  where (name:conds:_) = splitOn ": " str
        condParts = filter (/= "or") $ words conds
        parseCond str = (from, to)
          where (from:to:_) = map read $ splitOn "-" str
        rule = map parseCond condParts

passesRule :: Int -> [(Int, Int)] -> Bool
passesRule x conds = any id [x >= from && x <= to | (from, to) <- conds]

process :: [[String]] -> Int
process (rulesStr:yourTicketStr:nearbyTicketsStr:_) = sum $ map errorRate nearbyTickets
  where -- yourTicket = parseTicket $ head $ tail yourTicketStr
        nearbyTickets = map parseTicket $ tail nearbyTicketsStr
        parseTicket str = map read $ splitOn "," str
        rules = map parseRule rulesStr
        errorRate ticket = sum [field | field <- ticket, not (any (passesRule field) rules)]

main :: IO ()
main = do
  input <- getContents
  print $ process $ map lines $ splitOn "\n\n" input
