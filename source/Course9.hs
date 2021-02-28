{-
 - course9.hs
 -
 - Course work "Reverse interpolation problem"
 - Copyright (C) Pavel Kryukov, 2011
-}
module Main where

import System.Environment (getArgs)

import Course9.Core

main :: IO ()
main = getArgs >>= (print . getSolution)
