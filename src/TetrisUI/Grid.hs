module TetrisUI.Grid where
import UI.NCurses

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


