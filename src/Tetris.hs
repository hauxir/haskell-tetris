module Tetris(
    newGame,
    randomShape,
    update,
    addBlock,
    dropBlock,
    speedUp,
    moveRight,
    moveLeft,
    rotate,
    score,
    gameOver,
    Grid,
    Row,
    Block(..),
    Shape(..)
) where

import Data.List
import Data.Maybe
import System.Random

data Shape = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

data Block = Block { shape :: Shape, moving::Bool, origin::Bool}
            deriving (Eq, Show)

type Row = [Maybe Block]

type Grid = [Row]

newGame :: Grid
newGame = replicate gridHeight (replicate gridWidth Nothing)

randomShape :: RandomGen g => g -> (Shape, g)
randomShape g = case randomR (0,length [J ..]-1) g of (r, g') -> (toEnum r, g')

update :: Grid -> Shape -> Grid
update state = addBlock (gravitate (clearLines (freezeBlocks state)))

addBlock :: Grid -> Shape -> Grid
addBlock rows shape | empty rows && not (gameOver rows) = createShape shape ++ tail (tail (tail (tail rows)))
                    | otherwise = rows

dropBlock :: Grid -> Grid
dropBlock rows | gravitate rows /= rows = dropBlock (gravitate rows)
               | otherwise = rows

speedUp :: Grid -> Grid
speedUp = gravitate

moveRight :: Grid -> Grid
moveRight rows | not(touchright rows) = transpose (gravitate (transpose rows))
               | otherwise = rows
        where
            touchright :: Grid -> Bool
            touchright rows = any moving (mapMaybe last rows)

moveLeft :: Grid -> Grid
moveLeft rows | not(touchleft rows) = map reverse (transpose (gravitate (transpose (map reverse rows))))
              | otherwise = rows
        where
            touchleft :: Grid -> Bool
            touchleft rows = any moving (mapMaybe head rows)

rotate :: Grid -> Grid
rotate grid = insertRotated' (clearGrid grid) (rotateBlock grid) (map (getBlock grid) (movingCoordinates grid))
    where
        insertRotated':: Grid -> [(Int,Int)] -> [Maybe Block] -> Grid
        insertRotated' grid [] _ = grid
        insertRotated' grid (h:t) (val:valt) = insertRotated' (setBlock grid h val) t valt

        clearGrid :: Grid -> Grid
        clearGrid grid = clearGrid' grid (movingCoordinates grid)
            where
                clearGrid' :: Grid -> (Int,Int) -> Grid
                clearGrid' = foldl (\ grid h -> setBlock grid h Nothing)

        movingCoordinates :: Grid -> [(Int,Int)]
        movingCoordinates [] = []
        movingCoordinates (h:t) = movingCoordinates' h 25 - length t  ++ movingCoordinates t
            where
                movingCoordinates' :: Row -> Int -> [(Int,Int)]
                movingCoordinates' [] _ = []
                movingCoordinates' (h:t) y | movingBlock h = (y,9- length t):movingCoordinates' t y
                                        | otherwise = movingCoordinates' t y

        getOrigin::Grid -> (Int,Int)
        getOrigin grid = head (origins grid)

        isOrigin:: Grid -> (Int,Int) -> Bool
        isOrigin grid (x,y) = isJust (getBlock grid (x,y)) && origin (fromJust (getBlock grid (x,y)))

        origins:: Grid -> [(Int,Int)]
        origins grid = filter (isOrigin grid) (movingCoordinates grid)

        rotateBlock:: Grid -> [(Int,Int)]
        rotateBlock grid | hasOrigin grid 
                        && all (unoccupied grid) (map (rotatePoint (getOrigin grid)) (movingCoordinates grid))
                        = map (rotatePoint (getOrigin grid)) (movingCoordinates grid)
                         | otherwise = movingCoordinates grid

        rotatePoint::(Int,Int) -> (Int,Int) -> (Int,Int)
        rotatePoint (originx,originy) (x,y) = (originx + originy - y, originy - originx + x)

        hasOrigin::Grid -> Bool
        hasOrigin grid = null (origins grid)

        unoccupied::Grid -> (Int,Int) -> Bool
        unoccupied grid (x,y) = (x > 0 && x < gridHeight && y > 0 && y < gridWidth) 
                     && not (stationaryBlock (getBlock grid (x,y)))

        getBlock :: Grid -> (Int,Int) -> Maybe Block
        getBlock grid (x,y) = (grid !! x) !! y

        setBlock :: Grid -> (Int,Int) -> Maybe Block -> Grid
        setBlock grid (x,y) val =
                fst (splitAt x grid) ++ setBlock' (head (snd(splitAt x grid))) y val:tail(snd (splitAt x grid))
            where
                setBlock' :: Row -> Int -> Maybe Block -> Row
                setBlock' row y val = fst (splitAt y row) ++ val:tail(snd (splitAt y row))

score :: Grid -> Int
score state = product (replicate 2 (length (filter (==True) (map fullLine state))))

gameOver :: Grid -> Bool
gameOver state = any (not . all moving . catMaybes) (take 4 state)

---Helpers
gridHeight :: Int
gridHeight = 26

gridWidth:: Int
gridWidth = 10


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


clearLines :: Grid -> Grid
clearLines rows | empty rows = replicate (missing_rows rows) empty_row ++ remove_lines rows
                 | otherwise = rows
        where
              missing_rows :: Grid -> Int
              missing_rows rows = length rows - length (remove_lines rows)

              empty_row :: Row
              empty_row = replicate 10 Nothing

              remove_lines :: Grid -> Grid
              remove_lines = filter (not . fullLine)

fullLine :: Row -> Bool
fullLine line = filter (/= Nothing) line == line

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
                        o = block O True
              createT = 
                    [
                        [b,o,b],
                        [x,b,x]
                    ]
                    where
                        b = block T False
                        o = block T True

