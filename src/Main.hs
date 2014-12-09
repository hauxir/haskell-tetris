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

        draw 'I' = drawblock red
        draw 'S' = drawblock green
        draw 'O' = drawblock blue
        draw 'T' = drawblock yellow
        draw 'Z' = drawblock cyan
        draw 'J' = drawblock white
        draw 'L' = drawblock magenta
        draw 'M' = drawblock magenta
        draw _ = return ()

        drawLine [] y = return ()
        drawLine (head:tail) y = do
                                let x = (columns-((toInteger (length block))*(toInteger (length tail))))
                                moveCursor y (8+gridX+x+2)
                                draw head
                                drawLine tail y

        drawBlocks [] = return ()
        drawBlocks (head:tail) = do
                                    let y = (gridY+rows)-(toInteger (length tail))
                                    drawLine head y
                                    drawBlocks tail

    setCursorMode CursorInvisible
    setEcho False
    w <- defaultWindow
    let updateScreen gridlines = do
                        updateWindow w $ do
                            drawGrid gridY gridX gridcolor
                            drawBlocks gridlines
                        render
                        ev <- getEvent w (Just 100)
                        case ev of
                            Nothing -> updateScreen regular_update
                            Just ev' -> if ev' == (EventCharacter 'q')
                                        then return ()
                                   else if ev' == (EventSpecialKey KeyLeftArrow)
                                        then updateScreen (move_left regular_update)
                                   else if ev' == (EventSpecialKey KeyRightArrow)
                                        then updateScreen (move_right regular_update)
                                   else if ev' == (EventSpecialKey KeyDownArrow)
                                        then updateScreen (apply_gravity regular_update)
                                   else updateScreen regular_update
                            where
                                regular_update = add_block (clear_lines (freeze_blocks (apply_gravity (gridlines))))
    updateScreen gridRows
