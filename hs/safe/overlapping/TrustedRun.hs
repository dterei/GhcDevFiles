{-# LANGUAGE Trustworthy #-}
module Main where

import safe TrustedLib
import UntrustedPlug

main = do
    let r = res [(1::Int)]
    putStrLn $ "Result: " ++ show r 
    putStrLn $ "Result: " ++ show function

