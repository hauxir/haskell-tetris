module TetrisGame.Logic where
import Data.List
import Data.Maybe
import System.Random
import System.IO.Unsafe

randomStr :: String
randomStr = take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

data Shape = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

allShapes = [J ..]

data Block = Block { shape :: Shape, moving::Bool, origin::Bool}
            deriving (Eq, Show)

type Row = [Maybe Block]
type Grid = [Row]

newGame = replicate 26 (replicate 10 Nothing)

gravitate :: Grid -> Grid
gravitate rows | not(stopped rows) = transpose (gravitate_rows (transpose rows))
               | otherwise = rows
    where
        gravitate_row :: Row -> Row
        gravitate_row [] = []
        gravitate_row (Just (Block s True o):Nothing:t) = [Nothing,Just (Block s True o)] ++ t
        gravitate_row (Just (Block s True a):Just (Block _ True b):Just (Block _ True c):Just (Block _ True d):Nothing:t) = [Nothing,Just (Block s True a),Just (Block s True b),Just (Block s True c),Just (Block s True d)] ++ t
        gravitate_row (Just (Block s True a):Just (Block _ True b):Just (Block _ True c):Nothing:t) = [Nothing,Just (Block s True a),Just (Block s True b),Just (Block s True c)] ++ t
        gravitate_row (Just (Block s True a):Just (Block _ True b):Nothing:t) = [Nothing,Just (Block s True a),Just (Block s True b)] ++ t
        gravitate_row (h:t) = h : gravitate_row t

        gravitate_rows :: Grid -> Grid
        gravitate_rows [] = []
        gravitate_rows lis = gravitate_row (head lis) : gravitate_rows (tail lis)

stopped :: Grid -> Bool
stopped rows = any stopped' (transpose rows) || empty rows
    where
        stopped' :: Row -> Bool
        stopped' [] = False
        stopped' [Just (Block _ True _)] = True
        stopped' [Just (Block _ True _),Just (Block _ True _)] = True
        stopped' [Just (Block _ True _),Just (Block _ True _),Just (Block _ True _)] = True
        stopped' [Just (Block _ True _),Just (Block _ True _),Just (Block _ True _),Just (Block _ True _)] = True
        stopped' (Just (Block _ True _):Just (Block _ False _):t) = True
        stopped' (h:t) = stopped' t

empty :: Grid -> Bool
empty rows = all empty' (transpose rows)
    where
        empty' :: Row -> Bool
        empty' l | length (filter moving (catMaybes l)) == 0 = True
        empty' l = False

move_left :: Grid -> Grid
move_left rows | not(touchleft rows) = map reverse (transpose (gravitate (transpose (map reverse rows))))
               | otherwise = rows
        where
            touchleft :: Grid -> Bool
            touchleft rows = or(map moving (catMaybes(map head (rows))))

move_right :: Grid -> Grid
move_right rows | not(touchright rows) = transpose (gravitate (transpose rows))
                | otherwise = rows
        where
            touchright :: Grid -> Bool
            touchright rows = or(map moving (catMaybes(map last (rows))))


clear_lines :: Grid -> Grid
clear_lines rows | empty rows = (take (missing_rows rows) (repeat empty_row)) ++ (remove_lines rows)
                 | otherwise = rows
        where
              missing_rows :: Grid -> Int
              missing_rows rows = (length rows) - (length (remove_lines rows))

              empty_row :: Row
              empty_row = take 10 (repeat Nothing)

              remove_lines :: Grid -> Grid
              remove_lines rows = filter (not . full_line) rows

              full_line :: Row -> Bool
              full_line line = (filter (/= Nothing) line) == line

freeze_blocks :: Grid -> Grid
freeze_blocks rows | stopped rows = map freeze_blocks' rows
                   | otherwise = rows
            where
                freeze_blocks' :: Row -> Row
                freeze_blocks' [] = []
                freeze_blocks' (Just (Block s True o):t) = (Just (Block s False o):(freeze_blocks' t))
                freeze_blocks' b  = (head b):freeze_blocks' (tail b)

create_shape sh
              | sh == T =
                    pad [
                        [t,t,t],
                        [x,t,x]
                    ]
              | sh == I =
                    pad [
                        [x,i,x],
                        [x,i,x],
                        [x,i,x],
                        [x,i,x]
                    ]
              | sh == J =
                    pad [
                        [x,j,x],
                        [x,j,x],
                        [j,j,x]
                    ]
              | sh == L =
                    pad [
                        [x,l,x],
                        [x,l,x],
                        [x,l,l]
                    ]
              | sh == S =
                    pad [
                        [x,s,s],
                        [s,s,x]
                    ]
              | sh == Z =
                    pad [
                        [z,z,x],
                        [x,z,z]
                    ]
              | sh == O =
                    pad [
                        [x,o,o],
                        [x,o,o]
                    ]
        where
              t = block T
              j = block J
              i = block I
              l = block L
              z = block Z
              s = block S
              o = block O
              block shape  = Just (Block shape True False)
              x = Nothing
              pad s | length s == 2 = [(replicate 10 x)] ++ (map hpad s) ++ [(replicate 10 x)]
                    | length s == 3 = [(replicate 10 x)] ++ (map hpad s)
                    | otherwise = (map hpad s)
              hpad l = (replicate 3 x) ++ l ++ (replicate 4 x)

add_block :: Grid -> StdGen -> Grid
add_block rows rgen | empty rows = (create_shape (allShapes !! (fst (randomR (0,(length allShapes)-1) rgen)))) ++ (tail (tail (tail (tail rows))))
                    | otherwise = rows


drop_block :: Grid -> Grid
drop_block rows | (gravitate rows /= rows) = drop_block (gravitate rows)
                | otherwise = rows


tetrisUpdate :: Grid -> StdGen -> Grid
tetrisUpdate state g = add_block (gravitate (clear_lines (freeze_blocks state))) g

tetrisSpeedup :: Grid -> Grid
tetrisSpeedup state = gravitate state

tetrisMoveRight :: Grid -> Grid
tetrisMoveRight state = move_right state

tetrisMoveLeft :: Grid -> Grid
tetrisMoveLeft state = move_left state

tetrisDropblock :: Grid -> Grid
tetrisDropblock state = drop_block state

tetrisRotate :: Grid -> Grid
tetrisRotate state = state
