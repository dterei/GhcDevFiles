{-# LANGUAGE BangPatterns #-}

{-
    This is a bug program as under the LLVM backend it
    is slower than the C and NCG. The slow down appears
    to be due to 'opt' doing some bad stuff.

    http://hackage.haskell.org/trac/ghc/ticket/4223 

    ghc 6.12.1 -O2
    1.752
-}

import Data.Vector.Storable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.Types

-- Define a 4 element vector type
data Vec4 = Vec4 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat

------------------------------------------------------------------------

-- Ensure we can store it in an array
instance Storable Vec4 where
  sizeOf _ = sizeOf (undefined :: CFloat) * 4
  alignment _ = alignment (undefined :: CFloat)

  {-# INLINE peek #-}
  peek p = do
             a <- peekElemOff q 0
             b <- peekElemOff q 1
             c <- peekElemOff q 2
             d <- peekElemOff q 3
             return (Vec4 a b c d)
    where
      q = castPtr p

  {-# INLINE poke #-}
  poke p (Vec4 a b c d) = do
                            pokeElemOff q 0 a
                            pokeElemOff q 1 b
                            pokeElemOff q 2 c
                            pokeElemOff q 3 d
    where
      q = castPtr p

------------------------------------------------------------------------

a = Vec4 0.2 0.1 0.6 1.0
m = Vec4 0.99 0.7 0.8 0.6

add :: Vec4 -> Vec4 -> Vec4
{-# INLINE add #-}
add (Vec4 a b c d) (Vec4 a' b' c' d') = Vec4 (a+a') (b+b') (c+c') (d+d')

mult :: Vec4 -> Vec4 -> Vec4
{-# INLINE mult #-}
mult (Vec4 a b c d) (Vec4 a' b' c' d') = Vec4 (a*a') (b*b') (c*c') (d*d')

vsum :: Vec4 -> CFloat
{-# INLINE vsum #-}
vsum (Vec4 a b c d) = a+b+c+d

multList :: Int -> Vector Vec4 -> Vector Vec4
multList !count !src
    | count <= 0    = src
    | otherwise     = multList (count-1) $ V.map (\v -> add (mult v m) a) src

main = do
    print $ Data.Vector.Storable.sum
          $ Data.Vector.Storable.map vsum
          $ multList repCount
          $ Data.Vector.Storable.replicate arraySize (Vec4 0 0 0 0)

repCount, arraySize :: Int
repCount = 10000
arraySize = 20000
