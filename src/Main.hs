import UI.NCurses
import TetrisUI.Grid
import TetrisGame.Logic
import System.Exit


gridX = 50
gridY = 4

rows = 22
columns = 10


main :: IO ()
main = runCurses $ do
    gridcolor <- gridColor
    red <- newColorID ColorRed ColorRed 2
    green <- newColorID ColorGreen ColorGreen 3
    blue <- newColorID ColorBlue ColorBlue 4
    yellow <- newColorID ColorYellow ColorYellow 5
    cyan <- newColorID ColorCyan ColorCyan 6
    white <- newColorID ColorWhite ColorWhite 7
    magenta <- newColorID ColorMagenta ColorMagenta 8
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
        drawBlocks (head:tail) = do
                                    let y = (gridY+rows)-(toInteger (length tail))
                                    drawLine head y
                                    drawBlocks tail

    setCursorMode CursorInvisible
    setEcho False
    setCBreak True
    w <- defaultWindow
    let updateScreen gameState = do
                        updateWindow w $ do
                            drawBlocks gameState
                        render
                        ev <- getEvent w (Just 1000)
                        case ev of
                            Nothing -> updateScreen (tetrisUpdate gameState)
                            Just ev' -> if ev' == (EventCharacter 'q')
                                        then return ()
                                   else if ev' == (EventSpecialKey KeyLeftArrow)
                                        then updateScreen (tetrisMoveLeft gameState)
                                   else if ev' == (EventSpecialKey KeyRightArrow)
                                        then updateScreen (tetrisMoveRight gameState)
                                   else if ev' == (EventSpecialKey KeyDownArrow)
                                        then updateScreen (tetrisSpeedup gameState)
                                   else if ev' == (EventSpecialKey KeyUpArrow)
                                        then updateScreen (tetrisRotate gameState)
                                   else if ev' == (EventCharacter ' ')
                                        then updateScreen (tetrisDropblock gameState)
                                   else updateScreen (tetrisUpdate gameState)
    updateWindow w $ do
        drawGrid gridY gridX gridcolor
    render
    updateScreen (tetrisUpdate newGame)
