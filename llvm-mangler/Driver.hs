module Main ( main ) where

import System.Environment
import LlvmMangler
import Criterion.Main

main :: IO ()
main = do
    xs <- getArgs
    case xs of
         [x] -> llvmFixupAsm x (x ++ ".fixed.s")
         _   -> error "Bad number of arguments!\nUsage: $ program <filename>"

{-
main :: IO ()
main = defaultMain [ bench "mangler" $ llvmFixupAsm "WordsVect.s" "Main.fixed.s" ]
-}

