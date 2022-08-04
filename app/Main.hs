module Main where

import Ribbons

main :: IO ()
main =
  do putStrLn . show $ f <> g
    where a = Ribbon (Interval 'a' 0 50) (Interval 'b' 100 150)
          b = Ribbon (Interval 'a' 250 300) (Interval 'b' 150 200)
          c = Ribbon (Interval 'a' 300 350) (Interval 'b' 150 200)
          d = Ribbon (Interval 'b' 100 150) (Interval 'c' 0 50)
          e = Ribbon (Interval 'b' 150 200) (Interval 'c' 0 50)
          f = RibbonSet [a, b, c]
          g = RibbonSet [d, e]
