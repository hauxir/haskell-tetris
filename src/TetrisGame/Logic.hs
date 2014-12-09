module TetrisGame.Logic where
import Data.List

newGame =
      [
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          [' ',' ',' ',' ',' ',' ',' ' ,' ',' ',' '],
          ['I','I',' ',' ',' ',' ',' ' ,' ',' ',' '],
          ['I','I','I','I',' ',' ','I' ,'I','I','I'],
          ['I','I','I','I',' ',' ','I' ,'I','I','I']
      ]


gravitate :: [[Char]] -> [[Char]]
gravitate rows | not(stopped rows) = transpose (gravitate_rows (transpose rows))
               | otherwise = rows
    where
        gravitate_row :: [Char] -> [Char]
        gravitate_row [] = []
        gravitate_row ('M':' ':t) = [' ','M'] ++ t
        gravitate_row ('M':'M':'M':'M':' ':t) = [' ','M','M','M','M'] ++ t
        gravitate_row ('M':'M':'M':' ':t) = [' ','M','M','M'] ++ t
        gravitate_row ('M':'M':' ':t) = [' ','M','M'] ++ t
        gravitate_row (h:t) = h : gravitate_row t

        gravitate_rows :: [[Char]] -> [[Char]]
        gravitate_rows [] = []
        gravitate_rows lis = gravitate_row (head lis) : gravitate_rows (tail lis)

stopped :: [[Char]] -> Bool
stopped rows = any stopped' (transpose rows) || empty rows
    where
        stopped' :: [Char] -> Bool
        stopped' [] = False
        stopped' ['M'] = True
        stopped' ['M','M'] = True
        stopped' ['M','M','M'] = True
        stopped' ['M','M','M','M'] = True
        stopped' ('M':s:t) | s /= ' ' && s /= 'M' = True
        stopped' (h:t) = stopped' t

empty :: [[Char]] -> Bool
empty rows = all empty' (transpose rows)
    where
        empty' :: [Char] -> Bool
        empty' l | length (filter (=='M') l) == 0 = True
        empty' l = False

move_left :: [[Char]] -> [[Char]]
move_left rows | not(touchleft rows) = map reverse (transpose (gravitate (transpose (map reverse rows))))
               | otherwise = rows
        where
            touchleft :: [[Char]] -> Bool
            touchleft rows = or(map (=='M') (map head (rows)))

move_right :: [[Char]] -> [[Char]]
move_right rows | not(touchright rows) = transpose (gravitate (transpose rows))
                | otherwise = rows
        where
            touchright :: [[Char]] -> Bool
            touchright rows = or(map (=='M') (map last (rows)))


clear_lines :: [[Char]] -> [[Char]]
clear_lines rows | empty rows = (take (missing_rows rows) (repeat empty_row)) ++ (remove_lines rows)
                 | otherwise = rows
        where
              missing_rows :: [[Char]] -> Int
              missing_rows rows = (length rows) - (length (remove_lines rows))

              empty_row :: [Char]
              empty_row = take 10 (repeat ' ')

              remove_lines :: [[Char]] -> [[Char]]
              remove_lines rows = filter (not . full_line) rows

              full_line :: [Char] -> Bool
              full_line line = (filter (/= ' ') line) == line

freeze_blocks :: [[Char]] -> [[Char]]
freeze_blocks rows | stopped rows = map freeze_blocks' rows
                   | otherwise = rows
            where
                freeze_blocks' :: [Char] -> [Char]
                freeze_blocks' [] = []
                freeze_blocks' ('M':t) = ('I':(freeze_blocks' t))
                freeze_blocks' b  = (head b):freeze_blocks' (tail b)

add_block :: [[Char]] -> [[Char]]
add_block rows | empty rows = [
                                   [' ',' ',' ',' ','M','M',' ' ,' ',' ',' '],
                                   [' ',' ',' ',' ','M','M',' ' ,' ',' ',' ']
                                ] ++ (tail (tail rows))
               | otherwise = rows

drop_block :: [[Char]] -> [[Char]]
drop_block rows | (gravitate rows /= rows) = drop_block (gravitate rows)
                | otherwise = rows

tetrisUpdate :: [[Char]] -> [[Char]]
tetrisUpdate state = gravitate (add_block (clear_lines (freeze_blocks state)))

tetrisSpeedup :: [[Char]] -> [[Char]]
tetrisSpeedup state = gravitate (tetrisUpdate state)

tetrisMoveRight :: [[Char]] -> [[Char]]
tetrisMoveRight state = move_right (tetrisUpdate state)

tetrisMoveLeft :: [[Char]] -> [[Char]]
tetrisMoveLeft state = move_left (tetrisUpdate state)

tetrisDropblock :: [[Char]] -> [[Char]]
tetrisDropblock state = drop_block (tetrisUpdate state)

tetrisRotateRight :: [[Char]] -> [[Char]]
tetrisRotateRight state = state

tetrisRotateLeft :: [[Char]] -> [[Char]]
tetrisRotateLeft state = state
printTetris state = mapM_ print state
