module TetrisCurses where
import UI.NCurses
import System.Random
import Control.Monad
import Data.Char
import Tetris

playGame :: IO ()
playGame = do
    g <- newStdGen
    runCurses $ do
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

            drawGrid :: Integer -> Integer -> ColorID -> Update()
            drawGrid y x c = do
                setColor c
                moveCursor y (x+2)
                drawString gridTop
                drawLines (y+1) (x+1)
                moveCursor (rows+y+1) (x+1)
                drawString gridBottom
                where
                    gridTop :: String
                    gridTop = "____________________"

                    gridMiddle :: String
                    gridMiddle = "|                    |"

                    gridBottom :: String
                    gridBottom = " -------------------- "

                    drawLines :: Integer -> Integer -> Update()
                    drawLines y x = drawLines' y x rows
                        where
                            drawLines' :: Integer -> Integer -> Integer -> Update()
                            drawLines' y x n | n > 0 = do
                                moveCursor y x
                                drawString gridMiddle
                                drawLines' (y+1) x (n-1)
                                             | otherwise = return()
            drawBlocks :: Grid -> Update()
            drawBlocks [] = return ()
            drawBlocks (head:tail) | length (head:tail) <= fromIntegral rows = do
                                        let y = (gridY+rows)- toInteger (length tail)
                                        drawLine head y
                                        drawBlocks tail
                                   | otherwise = drawBlocks tail
                where
                    drawLine :: Row -> Integer -> Update()
                    drawLine [] y = return ()
                    drawLine (head:tail) y = do
                            let x = columns-(toInteger (length block)* toInteger (length tail))
                            moveCursor y (gridX+x+columns)
                            draw head
                            drawLine tail y
                        where
                            drawBlock :: ColorID -> Update()
                            drawBlock color = do
                                setColor color
                                drawString block

                            draw :: Maybe Block -> Update()
                            draw (Just (Block I _ _)) = drawBlock red
                            draw (Just (Block S _ _)) = drawBlock green
                            draw (Just (Block O _ _)) = drawBlock blue
                            draw (Just (Block T _ _)) = drawBlock yellow
                            draw (Just (Block Z _ _)) = drawBlock cyan
                            draw (Just (Block J _ _)) = drawBlock white
                            draw (Just (Block L _ _)) = drawBlock magenta
                            draw Nothing = drawBlock gridcolor

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
            drawScore score = do
                moveCursor (gridY - 1) (gridX + 1)
                setColor gridcolor
                let scorestr = show score
                drawString ("Score: " ++ scorestr)

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


            updateScreen :: Grid -> Int -> StdGen -> Int -> Curses()
            updateScreen gameState currentScore gen lvl = do
                updateWindow w $ do
                    drawBlocks gameState
                    drawScore currentScore
                    drawLevel lvl
                    when (gameOver gameState) drawGameOver
                render
                ev <- getEvent w (Just ((1+(9-toInteger lvl))*100))
                case ev of
                    Nothing -> updateScreen state newScore gen' lvl
                    Just ev'
                        | ev' == EventCharacter 'q' -> return ()
                        | ev' == EventSpecialKey KeyLeftArrow
                            -> updateScreen (moveLeft state) newScore gen' lvl
                        | ev' == EventSpecialKey KeyRightArrow
                            -> updateScreen (moveRight state) newScore gen' lvl
                        | ev' == EventSpecialKey KeyDownArrow
                            -> updateScreen (speedUp state) newScore gen' lvl
                        | ev' == EventSpecialKey KeyUpArrow
                            -> updateScreen (rotate state) newScore gen' lvl
                        | ev' == EventCharacter ' '
                            -> updateScreen (dropBlock state) newScore gen' lvl
                        | ev' == EventCharacter 'r'
                            -> game
                        | otherwise -> updateScreen state newScore gen' lvl
                    where
                        nextshape :: Shape
                        nextshape = fst (randomShape gen)

                        gen':: StdGen
                        gen' = snd (randomShape gen)

                        state :: Grid
                        state = update gameState nextshape

                        newScore :: Int
                        newScore = currentScore + (score gameState*lvl)

            game :: Curses()
            game = do
                updateWindow w $ drawGrid gridY gridX gridcolor
                updateWindow w levelMenu
                updateWindow w clearStats
                render
                ev <- getEvent w Nothing
                case ev of
                    Nothing -> game
                    Just (EventCharacter c) | isNumber c -> updateScreen newGame 0 g (digitToInt c)
                                            | c == 'q' -> return ()
                    Just _ -> game

        setCursorMode CursorInvisible
        setEcho False
        game
