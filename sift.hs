import Data.List

type Query  = String
type Choice = String
type Score  = Int

data Match =  Found String String Int Int
            -- | Found String String Int Int
            -- | Sequential String String Int Int
            -- | Boundary String String Int Int

infinity :: Int
infinity = 10000000

-- matches :: Query -> [Choice] -> [Choice]
-- matches query choices = map (\x -> ((lowestScoreForChoice query x), x)) choices

-- rankChoices :: Query -> [Choice] -> Choice
-- rankChoices query choices = map (\x -> (wordScores query x)) choices

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
  | q == i = wordScore (Found qs is (score+index) 1)
  | otherwise = wordScore (Found (q:qs) is score (index+1))
wordScore (Found [] _ score _) = score
wordScore (Found _ [] _ _) = infinity

main :: IO()
main = --do
  print $ lowestScore "ama" "app/models/america"
  -- print $  wordScore (NotFound "ama" "app/modules/america" 1 0) -- -> 2, 3, 11, 7
  -- print $  wordScore (NotFound "ame" "app/modules/america" 1 0) -- -> 3, 5
  -- print $  wordScore (NotFound "amep" "app/modules/america" 1 0) -- -> infinity
  -- print $  wordScore "amer" "app/modules/america" 1 0
  -- print $  wordScore "ami" "app/modules/america" 1 0
  -- print $  wordScore "amn" "app/modules/nepal" 1 0
  -- print $  wordScore "amp" "app/modules/nepal" 1 0
  -- print $  wordScore "ame" "app/modules/america" 0 0
  -- print $ read $ wordScore "ame" "app/modules/america" 0 0
  -- input_lines <- getContents
  -- tty <- openFile "/dev/tty" ReadMode
  -- putStr input_lines
  -- selection <- hGetLine tty
  -- print selection
