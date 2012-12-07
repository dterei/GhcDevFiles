module Main where

import qualified Data.Vector as U

main = print . U.sum $ res

res = U.replicate 100 $ U.sum $ U.enumFromTo 1 (1000000000 :: Int)

