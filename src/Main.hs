import UI.NCurses
import TetrisUI.Grid
import TetrisGame.Logic

gridX = 50
gridY = 4

rows = 25
columns = 10


main :: IO ()
main = runCurses $ do
    gridcolor <- gridColor
    red <- newColorID ColorRed ColorRed 2
    green <- newColorID ColorGreen ColorGreen 3
    blue <- newColorID ColorBlue ColorBlue 4
    yellow <- newColorID ColorYellow ColorYellow 5
    let
        drawblock color = do
                        setColor color
                        drawString block

        draw 'I' = drawblock red
        draw 'S' = drawblock green
        draw 'O' = drawblock blue
        draw 'T' = drawblock yellow
        draw _ = return ()

        drawLine [] y = return ()
        drawLine (head:tail) y = do
                                let x = (columns-((toInteger (length block))*(toInteger (length tail))))
                                moveCursor y (8+gridX+x+2)
                                draw head
                                drawLine tail y

        drawBlocks [] = return ()
        drawBlocks (head:tail) = do
                                    let y = ((rows-1)-(toInteger (length tail)))
                                    drawLine head y
                                    drawBlocks tail
    setCursorMode CursorInvisible
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        drawGrid gridY gridX gridcolor
        drawBlocks gridRows
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
