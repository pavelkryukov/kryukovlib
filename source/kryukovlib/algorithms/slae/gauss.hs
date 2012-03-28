{-
 - algorithms/slae/gauss.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.SLAE.Gauss
    (gauss)
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

import KryukovLib.Generic.Peano

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number

import KryukovLib.Types.Vector
import KryukovLib.Types.Matrix

import KryukovLib.Algorithms.SLAE

-- |Equation is the numbers in one line of matrix
-- and corresponding element of vector, i. e.
-- ax + by = c -> [a,b,c]
type Equation t = [t]

-- |Converter of System type to list of Equations
systemToEquations :: (Peano s) => SLAE s t -> [Equation t]
systemToEquations (SLAE (Matrix m) v) =
    systemToEquations' m (vtl v)
    where
        systemToEquations' :: [Vector s t] -> [t] -> [Equation t]
        systemToEquations' (a:at) (b:bt) =
            ((vtl a) ++ [b]) : systemToEquations' at bt
        systemToEquations' [] [] = []
        systemToEquations' []  _ =
            error "Size of matrix and vector are not the same"
        systemToEquations' _  [] =
            error "Size of matrix and vector are not the same"

-- |Gaussian elimination of system of equations
-- Algorithm is based on following:
-- http://luckytoilet.wordpress.com/2010/02/21/solving-systems-of-linear-equations-in-haskell/
elimination :: (Number t) => 
    [Equation t] -> Maybe [Equation t]
elimination matrix =
    case foldl reduceRow (Just matrix) [0..length matrix-1] of
        Just m -> Just (fixlastrow m)
        Nothing -> Nothing
    where
        --swaps element at position a with element at position b.
        swap xs a b
            | a > b = swap xs b a
            | a == b = xs
            | otherwise =
                let
                    (p1,p2) = splitAt a xs
                    (p3,p4) = splitAt (b - a - 1) (tail p2)
                in
                    p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)

        reduceRow Nothing _        = Nothing
        reduceRow (Just matrix1) r =
            let
                -- greatest element on or below (r,r).
                currentrow =
                    map
                        (\x -> norm2 (matrix1 !! x !! r))
                        [r..length matrix1 - 1]

                greatest =
                    case (elemIndex (maximum currentrow) currentrow) of
                        Just r' -> Just (r + r')
                        Nothing -> Nothing

                --matrix with row swapped (if needed)
                matrix2 =
                    case greatest of
                        Just greatest' -> Just (swap matrix1 r greatest')
                        Nothing -> Nothing

                --row we're working with
                row = (fromJust matrix2) !! r

                --make it have 1 as the leading coefficient
                row1 = map (\x -> x / (row !! r)) row

                --subtract nr from row1 while multiplying
                subrow nr =
                    let
                        k = nr!!r
                    in
                        zipWith (\a b -> k*a - b) row1 nr

                --apply subrow to all rows below
                nextrows = map subrow $ drop (r+1) (fromJust matrix2)

            --concat the lists and repeat
            in
                case matrix2 of
                    Just matrix2' -> Just (take r matrix2' ++[row1]++ nextrows)
                    Nothing -> Nothing

        fixlastrow matrix' =
            let
                a = init matrix'
                row = last matrix'
                z = last row
                nz = last (init row)
            in
                a ++ [init (init row) ++ [1, z / nz]]

-- |Solve a matrix (must already be in REF form) by back substitution.
substitute :: (Peano s, Number t) => Maybe [Equation t] -> Maybe (Vector s t)
substitute Nothing       = Nothing
substitute (Just matrix) =
    Just (Vector (foldr next [last (last matrix)] (init matrix)))
        where
            next row found =
                let
                    subpart = init $ drop (length matrix - length found) row
                    solution = last row - sum (zipWith (*) found subpart)
                in
                    solution : found

-- |Gauss method of solving SLAE
gauss :: (Peano s, Number t) => SLAESolver s t
gauss = substitute . elimination . systemToEquations