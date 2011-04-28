import qualified Data.Vector.Unboxed as U
import Data.Bits
import Criterion.Main

main' n = U.sum $ U.zipWith3 (\x y z -> x * y * z)
                            (U.enumFromTo 1 (n :: Int))
                            (U.enumFromTo 2 ((n+1) :: Int))
                            (U.enumFromTo 7 ((n+8) :: Int))

main = defaultMain [bench "zipWith3 (llvm)" $ whnf main' 100000000]
-- main = print $ main' 1
