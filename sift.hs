import Data.List
import System.IO
import System.Environment

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
lowestScore query choice =
  case wordScores query choice of
    []     -> (choice, infinity)
    scores -> (choice, minimum scores)

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
  | i == '.'              = wordScore (Boundary (q:qs) is score (index+1))
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

splitInput :: String -> [Choice]
splitInput input =
  let allLines = lines input
      result = allLines
  in result

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String ()
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>> ") evalAndPrint

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ head args
            otherwise -> putStrLn "Program takes only 0 or 1 argument"

-- main :: IO()
-- main = do
--   contents <- getContents
--   tty <- openFile "/dev/tty" ReadMode
--   putStr contents
--   selection <- hGetLine tty
--   putStrLn $ bestChoice selection (splitInput contents)
