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
persistScores = writeFile scoreFilePath . show . take maxScores . filter (> 0)

scoreFilePath :: FilePath
scoreFilePath = "scores"

maxScores :: Int
maxScores = 10

parseScores :: String -> [Int]
parseScores s
  | null readses = []
  | otherwise = fst . head $ readses
  where
    readses = reads s
