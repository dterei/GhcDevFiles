{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module TrustedLib (Pos(..)) where

class Pos a where
    res :: a -> Bool

instance Pos [a] where
    res _ = True

