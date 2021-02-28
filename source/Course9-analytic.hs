{-
 - course9-analytic.hs
 -
 - Course work "Reverse interpolation problem"
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module Main where

import Course9.Analytic

main :: IO ()
main = foldl (>>) (putStrLn "analytic") (map analyse [1..5])
