import Data.List

type Query  = String
type Choice = String
type Score  = Int

data Match =  Found String String Int Int
            | Boundary String String Int Int
            | Sequential String String Int Int

infinity :: Int
infinity = 10000000

bestChoice :: Query -> [Choice] -> Choice
bestChoice q cs = fst $ head $ rankChoices q cs

rankChoices :: Query -> [Choice] -> [(Choice, Score)]
rankChoices q cs = sortBy sortChoices $ scoreChoices q cs

sortChoices :: (Choice, Score) -> (Choice, Score) -> Ordering
sortChoices (_, scoreA) (_, scoreB)
  | scoreA <= scoreB = LT
  | scoreA > scoreB  = GT
  | otherwise        = GT

scoreChoices :: Query -> [Choice] -> [(Choice, Score)]
scoreChoices q = map (lowestScore q)

lowestScore :: Query -> Choice -> (Choice, Score)
lowestScore query choice = (choice, score)
  where
    score = minimum $ wordScores query choice

wordScores :: String -> String -> [Score]
wordScores [] _ = []
wordScores (q:qs) input = map score searchableWords
  where
    indices         = elemIndices q input
    searchableWords = map createChunk indices
    createChunk i   = drop i input
    score chunk     = wordScore (Found (q:qs) chunk 1 0)

wordScore :: Match -> Score
wordScore (Found (q:qs) (i:is) score index)
  | q == i && index == 1  = wordScore (Sequential qs is (score+1) index)
  | q == i                = wordScore (Found qs is (score+index) 1)
  | i == '/'              = wordScore (Boundary (q:qs) is score (index+1))
  | otherwise             = wordScore (Found (q:qs) is score (index+1))
wordScore (Found [] _ score _)      = score
wordScore (Boundary [] _ score _)   = score
wordScore (Sequential [] _ score _) = score
wordScore (Found _ [] _ _)          = infinity
wordScore (Boundary _ [] _ _)       = infinity
wordScore (Sequential _ [] _ _)     = infinity

wordScore (Boundary (q:qs) (i:is) score index)
  | q == i    = wordScore (Found qs is (score+1) 1)
  | otherwise = wordScore (Found (q:qs) is score (index+1))

wordScore(Sequential (q:qs) (i:is) score index)
  | q == i    = wordScore (Sequential qs is score 1)
  | otherwise = wordScore (Found (q:qs) is score (index+1))

main :: IO()
main = do
  print $ lowestScore "ama" "app/models/america" -- ->3
  print $ lowestScore "ame" "app/models/america"-- ->2
  print $ lowestScore "amer" "app/models/america"-- ->2
  print $ lowestScore "ami" "app/models/america"-- ->5
  print $ lowestScore "amn" "app/models/nepal"-- ->3
  print $ lowestScore "amp" "app/models/nepal"-- ->11
  print $ bestChoice "ama" ["app/models/america", "app/models/nepal"]
  print $ bestChoice "amn" ["app/models/america", "app/models/nepal"]
  print $ bestChoice "app/m/ep" ["app/models/america", "app/models/nepal"]
  print $ bestChoice "ama" ["app/models/america", "app/models/nepal", "app/models/animals"]
  print $ bestChoice "ami" ["app/models/america", "app/models/nepal", "app/models/animals"]
  print $ bestChoice "ani" ["app/models/america", "app/models/nepal", "app/models/animals"]
