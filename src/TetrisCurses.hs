module TetrisCurses where
import UI.NCurses
import Tetris
import System.Random

gridX = 50
gridY = 4

rows = 22
columns = 10

block = " ."
blocks n = concat $ replicate n block

gridLines = 22
gridColor = newColorID ColorBlue ColorDefault 1

gridTop :: String
gridTop = "____________________"

gridMiddle :: String
gridMiddle = "|                    |"

gridBottom :: String
gridBottom = " -------------------- "

drawLines :: Integer -> Integer -> Update()
drawLines y x = do drawLines' y x gridLines
           where
               drawLines' y x n | n > 0 = do
                                moveCursor y x
                                drawString gridMiddle
                                drawLines' (y+1) x (n-1)
                                | otherwise = return()


drawGrid :: Integer -> Integer -> ColorID -> Update()
drawGrid y x c = do
        setColor c
        moveCursor y (x+2)
        drawString gridTop
        drawLines (y+1) (x+1)
        moveCursor (gridLines+y+1) (x+1)
        drawString gridBottom

playGame :: IO ()
playGame = do
        rgen <- newStdGen
        runCurses $ do
            gridcolor <- gridColor
            red <- newColorID ColorRed ColorRed 2
            green <- newColorID ColorGreen ColorGreen 3
            blue <- newColorID ColorBlue ColorBlue 4
            yellow <- newColorID ColorYellow ColorYellow 5
            cyan <- newColorID ColorCyan ColorCyan 6
            white <- newColorID ColorWhite ColorWhite 7
            magenta <- newColorID ColorMagenta ColorMagenta 8
            redtext <- newColorID ColorRed ColorDefault 9
            let
                drawblock color = do
                                setColor color
                                drawString block

                draw (Just (Block I _ _)) = drawblock red
                draw (Just (Block S _ _)) = drawblock green
                draw (Just (Block O _ _)) = drawblock blue
                draw (Just (Block T _ _)) = drawblock yellow
                draw (Just (Block Z _ _))  = drawblock cyan
                draw (Just (Block J _ _))  = drawblock white
                draw (Just (Block L _ _))  = drawblock magenta
                draw Nothing = drawblock gridcolor

                drawLine [] y = return ()
                drawLine (head:tail) y = do
                                        let x = (columns-((toInteger (length block))*(toInteger (length tail))))
                                        moveCursor y (gridX+x+columns)
                                        draw head
                                        drawLine tail y

                drawBlocks [] = return ()
                drawBlocks (head:tail) | length (head:tail) <= fromIntegral rows = do
                                            let y = (gridY+rows)-(toInteger (length tail))
                                            drawLine head y
                                            drawBlocks tail
                drawBlocks (head:tail) = drawBlocks tail
                drawGameOver = do
                                moveCursor (gridY + (quot rows 2)) (gridX+8)
                                setColor redtext
                                drawString "         "
                                moveCursor (gridY + (quot rows 2)+1) (gridX+8)
                                drawString "GAME OVER!"
                                moveCursor (gridY + (quot rows 2)+2) (gridX+8)
                                drawString "         "

            setCursorMode CursorInvisible
            setEcho False
            setCBreak True
            w <- defaultWindow
            let updateScreen gameState gen = do
                                updateWindow w $ do
                                    drawBlocks gameState
                                    if gameover gameState then drawGameOver else return ()
                                render
                                ev <- getEvent w (Just 200)
                                case ev of
                                    Nothing -> updateScreen state gen'
                                    Just ev' -> if ev' == (EventCharacter 'q')
                                                then return ()
                                        else if ev' == (EventSpecialKey KeyLeftArrow)
                                                then updateScreen (tetrisMoveLeft state) gen'
                                        else if ev' == (EventSpecialKey KeyRightArrow)
                                                then updateScreen (tetrisMoveRight state) gen'
                                        else if ev' == (EventSpecialKey KeyDownArrow)
                                                then updateScreen (tetrisSpeedup state) gen'
                                        else if ev' == (EventSpecialKey KeyUpArrow)
                                                then updateScreen (tetrisRotate state) gen'
                                        else if ev' == (EventCharacter ' ')
                                                then updateScreen (tetrisDropblock state) gen'
                                        else updateScreen state gen'
                                    where
                                        nextshape = fst (randomShape gen)
                                        gen' = snd (randomShape gen)
                                        state = tetrisUpdate gameState nextshape
            updateWindow w $ do
                drawGrid gridY gridX gridcolor
            render
            updateScreen newGame rgen
