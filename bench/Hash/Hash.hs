{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSI
import Foreign.C
import System.Environment

foreign import ccall unsafe "c_hash.h c_hash" c_hash :: CString -> CLong -> IO CLong

hash_haskell :: BS.ByteString -> Int
hash_haskell str = BS.foldl' addWord8 0 str
  where addWord8 acc word = fromIntegral word `xor` (acc + acc `shiftL` 5)

hash_c :: BS.ByteString -> Int
hash_c str = fromIntegral $ BSI.inlinePerformIO $ BSU.unsafeUseAsCStringLen str $
              \(str, len) -> c_hash str (fromIntegral len)

num :: Int
num = 10^7

go :: Int -> (BS.ByteString -> Int) -> BS.ByteString -> Int -> Int
go i hash s acc | i `seq` hash `seq` acc `seq` False = undefined
go i hash s acc | i >= num = acc
go i hash s acc = go (i+1) hash s (acc + hash s)

main = do (which : len : _ ) <- getArgs
          let str = BSC.pack $ replicate (read len) 'a'
          print $ go 0 (if which == "c" then hash_c else hash_haskell) str 0
