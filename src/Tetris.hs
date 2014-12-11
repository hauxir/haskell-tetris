module Tetris where
import Data.List
import Data.Maybe
import System.Random

data Shape = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

data Block = Block { shape :: Shape, moving::Bool, origin::Bool}
            deriving (Eq, Show)

type Row = [Maybe Block]
type Grid = [Row]

randomShape :: RandomGen g => g -> (Shape, g)
randomShape g = case randomR (0,length [J ..]-1) g of (r, g') -> (toEnum r, g')

newGame :: Grid
newGame = replicate 26 (replicate 10 Nothing)

gravitate :: Grid -> Grid
gravitate rows | not(stopped rows) = transpose (gravitate_rows (transpose rows))
               | otherwise = rows
    where
        gravitate_row :: Row -> Row
        gravitate_row [] = []
        gravitate_row row | movingBlock (head row) = move_blocks row
        gravitate_row (h:t) = h : gravitate_row t

        gravitate_rows :: Grid -> Grid
        gravitate_rows [] = []
        gravitate_rows lis = gravitate_row (head lis) : gravitate_rows (tail lis)

        move_blocks :: Row -> Row
        move_blocks l | is_gap (gap l) = (Nothing:movingBlocks l) ++ tail (gap l) ++ ground l
            where
                is_gap :: Row -> Bool
                is_gap row = not (null (gap row)) && isNothing (head (gap row))

                movingBlocks :: Row -> Row
                movingBlocks (h:t) | movingBlock h = h:movingBlocks t
                movingBlocks _ = []

                gap:: Row -> Row
                gap (Nothing:t) = Nothing:gap' t
                    where
                        gap' (Nothing:t) = Nothing:gap' t
                        gap' _ = []

                gap (h:t) | movingBlock h = gap t
                gap _ = []

                ground :: Row -> Row
                ground [] = []
                ground (h:t) | stationaryBlock h = h:t
                             | otherwise = ground t


stopped :: Grid -> Bool
stopped rows = any stopped' (transpose rows) || empty rows
    where
        stopped' :: Row -> Bool
        stopped' [] = False
        stopped' row | all movingBlock row = True
        stopped' (first:second:_) | movingBlock first && stationaryBlock second = True
        stopped' (_:t) = stopped' t

movingBlock :: Maybe Block -> Bool
movingBlock block = isJust block && moving (fromJust block)

stationaryBlock :: Maybe Block -> Bool
stationaryBlock block = isJust block && not (moving (fromJust block))

empty :: Grid -> Bool
empty rows = all empty' (transpose rows)
    where
        empty' :: Row -> Bool
        empty' l | not (any moving (catMaybes l)) = True
        empty' l = False

moveLeft :: Grid -> Grid
moveLeft rows | not(touchleft rows) = map reverse (transpose (gravitate (transpose (map reverse rows))))
               | otherwise = rows
        where
            touchleft :: Grid -> Bool
            touchleft rows = any moving (mapMaybe head rows)

moveRight :: Grid -> Grid
moveRight rows | not(touchright rows) = transpose (gravitate (transpose rows))
                | otherwise = rows
        where
            touchright :: Grid -> Bool
            touchright rows = any moving (mapMaybe last rows)

clearLines :: Grid -> Grid
clearLines rows | empty rows = replicate (missing_rows rows) empty_row ++ remove_lines rows
                 | otherwise = rows
        where
              missing_rows :: Grid -> Int
              missing_rows rows = length rows - length (remove_lines rows)

              empty_row :: Row
              empty_row = replicate 10 Nothing

              remove_lines :: Grid -> Grid
              remove_lines = filter (not . full_line)

              full_line :: Row -> Bool
              full_line line = filter (/= Nothing) line == line

freezeBlocks :: Grid -> Grid
freezeBlocks rows | stopped rows = map freezeBlocks' rows
                   | otherwise = rows
            where
                freezeBlocks' :: Row -> Row
                freezeBlocks' [] = []
                freezeBlocks' (Just (Block s True o):t) = Just (Block s False o): freezeBlocks' t
                freezeBlocks' b  = head b:freezeBlocks' (tail b)

createShape :: Shape -> Grid
createShape sh | sh == I = pad createI
               | sh == J = pad createJ
               | sh == L = pad createL
               | sh == S = pad createS
               | sh == Z = pad createZ
               | sh == O = pad createO
               | sh == T = pad createT
        where
              block shape origin = Just (Block shape True origin)
              x = Nothing
              hpad l = replicate 3 x ++ l ++ replicate 4 x

              pad s | length s == 2 = [replicate 10 x] ++ map hpad s ++ [replicate 10 x]
                    | length s == 3 = replicate 10 x : map hpad s
                    | otherwise = map hpad s

              createI =
                    [
                        [x,b,x],
                        [x,o,x],
                        [x,b,x],
                        [x,b,x]
                    ]
                    where
                        b = block I False
                        o = block I True

              createJ =
                    [
                        [x,b,x],
                        [x,o,x],
                        [b,b,x]
                    ]
                    where
                        b = block J False
                        o = block J True

              createL =
                    [
                        [x,b,x],
                        [x,o,x],
                        [x,b,b]
                    ]
                    where
                        b = block L False
                        o = block L True

              createS =
                    [
                        [x,b,b],
                        [b,o,x]
                    ]
                    where
                        b = block S False
                        o = block S True

              createZ =
                    [
                        [b,b,x],
                        [x,o,b]
                    ]
                    where
                        b = block Z False
                        o = block Z True

              createO =
                    [
                        [x,b,b],
                        [x,b,b]
                    ]
                    where
                        b = block O False
              createT = 
                    [
                        [b,o,b],
                        [x,b,x]
                    ]
                    where
                        b = block T False
                        o = block T True

addBlock :: Grid -> Shape -> Grid
addBlock rows shape | empty rows && not (gameover rows) = createShape shape ++ tail (tail (tail (tail rows)))
                     | otherwise = rows

dropBlock :: Grid -> Grid
dropBlock rows | gravitate rows /= rows = dropBlock (gravitate rows)
                | otherwise = rows

tetrisUpdate :: Grid -> Shape -> Grid
tetrisUpdate state = addBlock (gravitate (clearLines (freezeBlocks state)))

tetrisSpeedup :: Grid -> Grid
tetrisSpeedup = gravitate

tetrisMoveRight :: Grid -> Grid
tetrisMoveRight = moveRight

tetrisMoveLeft :: Grid -> Grid
tetrisMoveLeft = moveLeft

tetrisDropblock :: Grid -> Grid
tetrisDropblock = dropBlock

tetrisRotate :: Grid -> Grid
tetrisRotate state = state

gameover :: Grid -> Bool
gameover state = any (not . all moving . catMaybes) (take 4 state)
