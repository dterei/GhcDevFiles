{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Main where

import Criterion.Main
import GHC.Exts
import GHC.ST

main :: IO ()
main = do
    defaultMain
        [ bench "thawArray/1" $ whnf (thawArray arr1 1) 10000
        , bench "thawArray/2" $ whnf (thawArray arr2 2) 10000
        , bench "thawArray/4" $ whnf (thawArray arr4 4) 10000
        , bench "thawArray/8" $ whnf (thawArray arr8 8) 10000
        , bench "thawArray/16" $ whnf (thawArray arr16 16) 10000
        , bench "thawArray/32" $ whnf (thawArray arr32 32) 10000
        ]
  where
    !arr1 = runST (new 1 0 >>= unsafeFreeze)
    !arr2 = runST (new 2 0 >>= unsafeFreeze)
    !arr4 = runST (new 4 0 >>= unsafeFreeze)
    !arr8 = runST (new 8 0 >>= unsafeFreeze)
    !arr16 = runST (new 16 0 >>= unsafeFreeze)
    !arr32 = runST (new 32 0 >>= unsafeFreeze)

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

thawArray :: Array Int -> Int -> Int -> ()
thawArray !arr (I# sz#) !n = runST $ ST $ \ s# -> go 0 s#
  where
    go i s#
        | i >= n = (# s#, () #)
        | otherwise = case thawArray# (unArray arr) 0# sz# s# of
            (# s2#, marr# #) -> go (i+1) s2#

