{-# LANGUAGE Safe #-}
module Main where

import Pos
import GHC.Generics

unsafeIfy :: Pos -> Pos
unsafeIfy inp = case from inp of
    M1 (M1 (M1 (K1 x))) -> to $ M1 (M1 (M1 (K1 (negate x))))

main = do
    let (Just x) = mkPos 2
    putStrLn "Should print \"Pos (-2)\""
    print $ unsafeIfy x
    putStrLn "If it printed Pos (-2), we've successfully violated the abstraction boundaries of module Pos"
