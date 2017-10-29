import TetrisCurses

main :: IO()
main = playGame [] >>= print
