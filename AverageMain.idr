module Main

import System.REPL
import Average

showAverage : String -> String
showAverage s = "The average is " ++ show (average s) ++ "\n"

main : IO()
main = repl "Enter a string: " showAverage
