{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module UntrustedPlug where

import TrustedLib

instance Pos [Int] where
    res _ = False

