import System.Directory
import TetrisCurses

main :: IO()
main = do
  scores <- unpersistScores
  newScores <- playGame scores
  persistScores newScores

unpersistScores :: IO [Int]
unpersistScores = do
  hasF <- doesFileExist scoreFilePath
  if not hasF then return [] else parseScores <$> readFile scoreFilePath

persistScores :: [Int] -> IO ()
persistScores [] = return ()
persistScores scores = writeFile scoreFilePath (show scores)

scoreFilePath :: FilePath
scoreFilePath = "scores"

parseScores :: String -> [Int]
parseScores s
  | null readses = []
  | otherwise = fst . head $ readses
  where
    readses = reads s
