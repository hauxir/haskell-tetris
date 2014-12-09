module TetrisGame.Logic where
import Data.List


gridRows =
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
          ['O','O',' ',' ',' ',' ',' ' ,' ',' ',' '],
          ['O','O','I','I','I',' ',' ' ,'I','T','I'],
          ['I','I','I','I','S',' ',' ' ,'T','T','T']
      ]

gravitate :: [Char] -> [Char]
gravitate [] = []
gravitate ('M':' ':t) = [' ','M'] ++ t
gravitate ('M':'M':'M':'M':' ':t) = [' ','M','M','M','M'] ++ t
gravitate ('M':'M':'M':' ':t) = [' ','M','M','M'] ++ t
gravitate ('M':'M':' ':t) = [' ','M','M'] ++ t
gravitate (h:t) = h:(gravitate t)

gravitate2 :: [[Char]] -> [[Char]]
gravitate2 [] = []
gravitate2 lis = (gravitate (head lis)):(gravitate2 (tail lis))


apply_gravity rows | not(stopped rows) = transpose (gravitate2 (transpose rows))
apply_gravity rows = rows

stopped rows = (or (map stopped' (transpose rows))) || (and(map empty' (transpose rows)))
stopped' [] = False
stopped' ['M'] = True
stopped' ['M','M'] = True
stopped' ['M','M','M'] = True
stopped' ['M','M','M','M'] = True
stopped' ('M':s:t) | s /= ' ' && s /= 'M' = True
stopped' (h:t) = stopped' t
empty' l | length (filter (=='M') l) == 0 = True
empty' l = False

touchleft rows = or(map (=='M') (map head (rows)))

touchright rows = touchleft (map reverse rows)

move_left rows | not(touchleft rows) = map reverse (transpose (apply_gravity (transpose (map reverse rows))))
move_left rows = rows

move_right rows | not(touchright rows) = transpose (apply_gravity (transpose rows))
move_right rows = rows

full_line line = (filter (/= ' ') line) == line

clear_lines rows | stopped(rows) = take ((length rows) - (length (remove_lines rows))) (repeat (take 10 (repeat ' '))) ++ (remove_lines rows)
        where 
              remove_lines rows = filter (not . full_line) rows
clear_lines rows = rows

freeze_blocks rows | stopped rows= map freeze_blocks' rows
            where
                freeze_blocks' [] = []
                freeze_blocks' ('M':t) = ('I':(freeze_blocks' t))
                freeze_blocks' b  = (head b):freeze_blocks' (tail b)
freeze_blocks rows = rows


add_block rows | stopped rows = [[' ',' ',' ',' ','M','M',' ' ,' ',' ',' '],[' ',' ',' ',' ','M','M',' ' ,' ',' ',' ']] ++ (tail (tail rows))
add_block rows = rows
