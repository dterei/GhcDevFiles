{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Main where

import Criterion.Main
import GHC.Exts
import GHC.ST
import Control.Exception

main :: IO ()
main = do
    defaultMain
        [ bench "thawArray" $ whnf thawArray arr
        ]
  where !arr = runST (new 32 0 >>= unsafeFreeze)

data Array a = Array { unArray :: !(Array# a) }
data MArray s a = MArray { unMArray :: !(MutableArray# s a) }

new :: Int -> a -> ST s (MArray s a)
new n@(I# n#) b = ST $ \s -> case newArray# n# b s of
    (# s', ary #) -> (# s', MArray ary #)
{-# INLINE new #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze (MArray mary)
    = ST $ \s -> case unsafeFreezeArray# mary s of
                   (# s', ary #) -> (# s', Array ary #)
{-# INLINE unsafeFreeze #-}

thawArray :: Array Int -> Array Int
thawArray arr = runST $ ST $ \ s# ->
    case thawArray# (unArray arr) 0# 32# s# of
        (# s2#, marr# #) -> case unsafeFreezeArray# marr# s2# of
            (# s3#, arr# #) -> (# s3#, Array arr# #)
