{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}

module Pos (
  Pos
, mkPos
) where

import GHC.Generics
newtype Pos = Pos Int deriving (Generic, Eq, Show)

mkPos :: Int -> Maybe Pos
mkPos x | x > 0 = Just (Pos x)
        | otherwise = Nothing

