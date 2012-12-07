module Main(main) where

import Data.Array.Base
import Data.Array.IO
import Data.Array.MArray

main :: IO ()
main = do
    arr <- newArray_ (0, 200)
    go arr 2 0 100

go :: IOUArray Int Int -> Int -> Int -> Int -> IO ()
go arr stride x y | x < y = do unsafeWrite arr (x * stride) 1337
                               go arr stride (x + 1) y
                  | otherwise = return ()

