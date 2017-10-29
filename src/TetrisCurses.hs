module TetrisCurses where

import Control.Monad
import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

playGame :: IO ()
playGame = newStdGen >>= \g -> runCurses $ do
  w <- defaultWindow
  gridcolor <- newColorID ColorBlue ColorDefault 1
  red <- newColorID ColorRed ColorRed 2
  green <- newColorID ColorGreen ColorGreen 3
  blue <- newColorID ColorBlue ColorBlue 4
  yellow <- newColorID ColorYellow ColorYellow 5
  cyan <- newColorID ColorCyan ColorCyan 6
  white <- newColorID ColorWhite ColorWhite 7
  magenta <- newColorID ColorMagenta ColorMagenta 8
  redtext <- newColorID ColorRed ColorDefault 9
  let
      draw :: Maybe Block -> Update()
      draw (Just (Block I _ _)) = drawBlock red
      draw (Just (Block S _ _)) = drawBlock green
      draw (Just (Block O _ _)) = drawBlock blue
      draw (Just (Block T _ _)) = drawBlock yellow
      draw (Just (Block Z _ _)) = drawBlock cyan
      draw (Just (Block J _ _)) = drawBlock white
      draw (Just (Block L _ _)) = drawBlock magenta
      draw Nothing = drawBlock gridcolor

      drawBlocks :: Grid -> Update()
      drawBlocks [] = return ()
      drawBlocks l@(h:t) = do
        when (length l <= fromIntegral rows) $ drawLine h y
        drawBlocks t
        where
          y = (gridY+rows)- toInteger (length t)

      drawLine :: Row -> Integer -> Update()
      drawLine [] _ = return ()
      drawLine (h:t) y = do
        let x = columns - (toInteger (length block) * toInteger (length t))
        moveCursor y $ gridX + x + columns
        draw h
        drawLine t y

      drawGameOver :: Update()
      drawGameOver = do
        moveCursor (gridY + quot rows 2) (gridX + 8)
        setColor redtext
        drawString "         "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "     GAME OVER!     "
        moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
        drawString " press 'r' to retry "

      drawScore :: Int -> Update()
      drawScore scoreValue = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        let scorestr = show scoreValue
        drawString ("Score: " ++ scorestr)

      drawHighScores :: [Int] -> Update ()
      drawHighScores scores = setColor gridcolor >> forM_ (zip [1..] scores) drawHighScore

      drawLevel :: Int -> Update()
      drawLevel level = do
        moveCursor (gridY - 1) (gridX + 15)
        setColor gridcolor
        drawString ("Level: " ++ show level)

      levelMenu = do
        setColor redtext
        drawString "                    "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "    Choose level:   "
        moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
        drawString "        0-9         "

      clearStats = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        drawString "                      "

      updateScreen :: Grid -> Int -> StdGen -> Int -> [Int] -> Bool -> Curses()
      updateScreen gameState currentScore gen lvl highScores updatable = do
        let
          gameEnded = gameOver gameState
          newHighScores
            | gameEnded && updatable = take 5 . reverse . sort $ currentScore : highScores
            | otherwise = highScores
          newUpd = not gameEnded
        updateWindow w $ do
          drawBlocks gameState
          drawScore currentScore
          drawLevel lvl
          when gameEnded drawGameOver
          drawHighScores newHighScores
        render
        ev <- getEvent w (Just ((1+(9-toInteger lvl))*100))
        case ev of
          Nothing -> updateScreen state newScore gen' lvl newHighScores newUpd
          Just ev'
            | ev' == EventCharacter 'q' -> return ()
            | ev' == EventSpecialKey KeyLeftArrow -> updateScreen (moveLeft state) newScore gen' lvl newHighScores newUpd
            | ev' == EventSpecialKey KeyRightArrow -> updateScreen (moveRight state) newScore gen' lvl newHighScores newUpd
            | ev' == EventSpecialKey KeyDownArrow -> updateScreen (speedUp state) newScore gen' lvl newHighScores newUpd
            | ev' == EventSpecialKey KeyUpArrow -> updateScreen (rotate state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter ' ' -> updateScreen (dropBlock state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'r' -> game newHighScores
            | otherwise -> updateScreen state newScore gen' lvl newHighScores newUpd
        where
          (nextshape, gen') = randomShape gen
          state = update gameState nextshape
          newScore = currentScore + (score gameState*(1+lvl))

      game :: [Int] -> Curses()
      game scores = do
        updateWindow w $ drawGrid gridY gridX gridcolor
        updateWindow w levelMenu
        updateWindow w clearStats
        updateWindow w $ drawHighScores scores
        render
        ev <- getEvent w Nothing
        case ev of
          Nothing -> game scores
          Just (EventCharacter c)
            | isNumber c -> updateScreen newGame 0 g (digitToInt c) scores True
            | c == 'q' -> return ()
          Just _ -> game scores

  _ <- setCursorMode CursorInvisible
  setEcho False
  game _s
  where
    _s = [3, 2, 1]

drawBlock :: ColorID -> Update()
drawBlock color = do
  setColor color
  drawString block

drawGrid :: Integer -> Integer -> ColorID -> Update()
drawGrid y x c = do
  setColor c
  moveCursor y (x+1)
  drawString gridTop
  drawLines (y+1) (x+1)
  moveCursor (rows+y+1) (x+1)
  drawString gridBottom

drawLines :: Integer -> Integer -> Update()
drawLines y x = drawLines' y x rows

drawLines' :: Integer -> Integer -> Integer -> Update()
drawLines' y x n
  | n < 1 = return()
  | otherwise = do
      moveCursor y x
      drawString gridMiddle
      drawLines' (y+1) x (n-1)

drawHighScore :: (Integer, Int) -> Update ()
drawHighScore (i, s) = do
  moveCursor (gridY + rows + 1 + i) (gridX + 6)
  drawString $ printf "%d.%10d" i s

gridTop, gridMiddle, gridBottom :: String
gridTop    = " ____________________ "
gridMiddle = "|                    |"
gridBottom = " -------------------- "

block :: String
block = " ."

gridX :: Integer
gridX = 50

gridY :: Integer
gridY = 4

rows :: Integer
rows = toInteger (length newGame - 4)

columns :: Integer
columns = toInteger (length (head newGame))
