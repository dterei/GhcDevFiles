import Data.Array.Vector
import Data.Bits

main = print . sumU $ zipWith3U (\x y z -> x * y * z)
                    (enumFromToU 1 (100000000 :: Int))
                    (enumFromToU 2 (100000001 :: Int))
                    (enumFromToU 7 (100000008 :: Int))

