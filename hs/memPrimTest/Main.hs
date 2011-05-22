{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

foreign import ccall "memintrinTest" memcpyTest :: IO ()

main = do
    putStrLn "Mem{cpy,set,move} Intrinsics Test..."
    _ <- memcpyTest
    putStrLn "Testing Done"
    return ()

