module Heaps where

import GHC.Prim

data I = I Int#
data F = F Float#

pp_terei :: Int -> Int -> Int
pp_terei x y = 2 * ( x + y )

uu_terei :: I -> I -> I
uu_terei x y = let !(I x') = x
                   !(I y') = y
               in I (2# *# (x' +# y'))

ufuf_terei :: F -> F -> F
ufuf_terei x y = let !(F x') = x
                     !(F y') = y
                 in F (x' `timesFloat#` y')

let_terei :: Int -> Int -> Int
let_terei x y = let addy z = z + y
                in y + (addy x) + x

ff_terei :: Float -> Float -> Float
ff_terei x y = x + y

pfp_terei :: Int -> Float -> Int -> Float
pfp_terei x y z = (fromIntegral x) * y + (fromIntegral z)

case_terei :: Maybe Int -> Maybe Int -> Int
case_terei x y = let x' = case x of
                            Just n  -> n
                            Nothing -> 0
                     y' = case y of
                            Just n  -> n
                            Nothing -> 1
                 in  x' + y'

big_terei :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
big_terei a b c d e f g h i j k l m = sum [a,b,c,d,e,f,g,h,i,j,k,l,m]

fun_terei :: (Int -> Int -> Int) -> Int -> Int
fun_terei f x = let y = f x 12
                in y + x + y + x

stat_thunk :: [Int]
stat_thunk = [1..]

dyn_thunk :: (Int -> Bool) -> Int -> Int
dyn_thunk p n = let xs = [ x | x <- [1..n], p x]
                in sum xs
