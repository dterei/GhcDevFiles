{-# LANGUAGE UnboxedTuples, MagicHash #-}
-- | Roman provided safe load example benchmark.
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM/Alias
module Main where

import GHC.Exts
import GHC.Prim

import Data.Primitive.ByteArray

-- | Run foo.
main :: IO ()
main = do
  let len = 10#
  ba            <- newByteArray $ I# len
  ByteArray ba' <- unsafeFreezeByteArray ba
  let (# _, l #) = foo ba' len
  putStrLn $ show l

-- | multiply each element of a byte array and count it's length
foo :: ByteArray# -> Int# -> (# Double#, Int #)
foo as n = loop 0# 0.0##
  where
    loop i x
      | i >=# n = (# x, I# i #)
      | otherwise = loop (i +# 1#) (x *## indexDoubleArray# as i)

