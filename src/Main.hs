import UI.NCurses
import TetrisUI.Grid
import TetrisGame.Logic
import System.Exit
import System.Random


gridX = 50
gridY = 4

rows = 22
columns = 10


main :: IO ()
main = do
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
