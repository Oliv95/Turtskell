module Main(main) where

import Turtskell

main :: IO()
main = runCommands (string2Commands (nth 6))

-- Translate a Char to a TurtleCommand
char2TC 'F' = Forward 5
char2TC '+' = Turn 90
char2TC '-' = Turn 270

--Translate a String to a [TurtleCommand]
string2Commands s = map char2TC s

axiom = "F"

--rules for the "koch curve"
rule 'F' = "F+F-F-F+F"
rule  c  = [c]

next :: String -> String
next s = concat $ map rule s

nth :: Int -> String
nth n = iterate next axiom !! n
