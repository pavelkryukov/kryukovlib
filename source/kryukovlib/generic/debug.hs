{-
 - generic/debug.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Generic.Debug
    (notImpl, tr)
where

import Debug.Trace

-- |Universal stub function
notImpl :: a
notImpl =
    error "KryukovLib: Not implemented yet"

-- |Sending value with tracing
tr :: (Show a) => a -> a
tr smth = trace (show smth) smth