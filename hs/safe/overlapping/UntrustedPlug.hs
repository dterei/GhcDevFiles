{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE Safe #-}
module UntrustedPlug where

import TrustedLib

instance Pos a where
    res _ = False

instance Pos [Int] where
    res _ = True

function :: Int
function = 3

