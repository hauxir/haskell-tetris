import UI.NCurses
import TetrisUI.Grid
import TetrisGame.Logic

gridX = 50
gridY = 4

drawblock 'O' = do
                setColor defaultColorID
                drawString block

drawblock 'I' = do
                setColor defaultColorID
                drawString block

drawblock 'S' = do
                setColor defaultColorID
                drawString block

drawblock 'T' = do
                setColor defaultColorID
                drawString block

main :: IO ()
main = runCurses $ do
    gridcolor <- gridColor
    red <- newColorID ColorRed ColorRed 2
    green <- newColorID ColorGreen ColorGreen 3
    blue <- newColorID ColorBlue ColorBlue 4
    yellow <- newColorID ColorYellow ColorYellow 5
    setCursorMode CursorInvisible
    setEcho False
    w <- defaultWindow
    updateWindow w $ do

        drawGrid gridY gridX gridcolor
        moveCursor (gridLines+gridY) (gridX+2)

        --I block
        setColor red
        drawString (blocks 4)

        --O block
        setColor blue
        moveCursor (gridLines+gridY-1) (gridX+2)
        drawString (blocks 2)
        moveCursor (gridLines+gridY-2) (gridX+2)
        drawString (blocks 2)

        --S block
        setColor green
        moveCursor (gridLines+gridY) (gridX+8)
        drawString (blocks 2)
        moveCursor (gridLines+gridY-1) (gridX+10)
        drawString (blocks 2)

        --T block
        setColor yellow
        moveCursor (gridLines+gridY) (gridX+14)
        drawString (blocks 3)
        moveCursor (gridLines+gridY-1) (gridX+16)
        drawString (blocks 1)

    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
