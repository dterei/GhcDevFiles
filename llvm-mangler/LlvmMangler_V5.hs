-- -----------------------------------------------------------------------------
-- | GHC LLVM Mangler
--
-- This script processes the assembly produced by LLVM, rearranging the code
-- so that an info table appears before its corresponding function. We also
-- use it to fix up the stack alignment, which needs to be 16 byte aligned
-- but always ends up off by 4 bytes because GHC sets it to the 'wrong'
-- starting value in the RTS.
--
-- We only need this for Mac OS X, other targets don't use it.
--

module LlvmMangler_V5 ( llvmFixupAsm ) where

-- #include "HsVersions.h"

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.IntMap as I
import System.IO

-- Magic Strings
infoSec, newInfoSec, newLine, spInst, jmpInst :: B.ByteString
infoSec    = B.pack "\t.section\t__STRIP,__me"
newInfoSec = B.pack "\n\t.text"
newLine    = B.pack "\n"
jmpInst    = B.pack "\n\tjmp"

infoLen, labelStart, spFix :: Int
infoLen    = B.length infoSec
labelStart = B.length jmpInst

spInst     = B.pack ", %rsp\n"
spFix      = 8

-- Search Predicates
eolPred, dollarPred, commaPred :: Char -> Bool
eolPred    = ((==) '\n')
dollarPred = ((==) '$')
commaPred  = ((==) ',')

-- | Read in assembly file and process
llvmFixupAsm :: FilePath -> FilePath -> IO ()
llvmFixupAsm f1 f2 = do
    r <- openBinaryFile f1 ReadMode
    w <- openBinaryFile f2 WriteMode
    fixTables r w I.empty
    B.hPut w (B.pack "\n\n")
    hClose r
    hClose w
    return ()

{- |
    Here we process the assembly file one function and data
    defenition at a time. When a function is encountered that
    should have a info table we store it in a map. Otherwise
    we print it. When an info table is found we retrieve its
    function from the map and print them both.

    For all functions we fix up the stack alignment. We also
    fix up the section defenition for functions and info tables.
-}
fixTables :: Handle -> Handle -> I.IntMap B.ByteString -> IO ()
fixTables r w m = do
    f <- getFun r B.empty
    if B.null f
       then return ()
       else let fun   = fixupStack f B.empty
                (a,b) = B.breakSubstring infoSec fun
                (x,c) = B.break eolPred b
                fun'  = a `B.append` newInfoSec `B.append` c
                n     = readInt $ B.drop infoLen x
                (bs, m') | B.null b  = ([fun], m)
                         | even n    = ([], I.insert n fun' m)
                         | otherwise = case I.lookup (n+1) m of
                               Just xf' -> ([fun',xf'], m)
                               Nothing  -> ([fun'], m)
            in mapM_ (B.hPut w) bs >> fixTables r w m'

-- | Read in the next function/data defenition
getFun :: Handle -> B.ByteString -> IO B.ByteString
getFun r f = do
    l <- (try (B.hGetLine r))::IO (Either IOError B.ByteString)
    case l of
        Right l' | B.null l' -> return f
                 | otherwise -> getFun r (f `B.append` newLine `B.append` l')
        Left _ -> return B.empty

{-|
    Mac OS X requires that the stack be 16 byte aligned when making a function
    call (only really required though when making a call that will pass through
    the dynamic linker). The alignment isn't correctly generated by LLVM as
    LLVM rightly assumes that the stack wil be aligned to 16n + 12 on entry
    (since the function call was 16 byte aligned and the return address should
    have been pushed, so sub 4). GHC though since it always uses jumps keeps
    the stack 16 byte aligned on both function calls and function entry.

    We correct the alignment here.
-}
fixupStack :: B.ByteString -> B.ByteString -> B.ByteString
fixupStack f f' | B.null f' =
    let -- fixup sub op
        (a, c) = B.breakSubstring spInst f
        (b, n) = B.breakEnd dollarPred a
        num    = B.pack $ show $ readInt n + spFix
    in if B.null c
          then f' `B.append` f
          else fixupStack c $ f' `B.append` b `B.append` num

fixupStack f f' =
    let -- fixup add ops
        (a, c)  = B.breakSubstring jmpInst f
        -- we matched on a '\n' so go past it
        (l', b) = B.break eolPred $ B.tail c
        l       = (B.head c) `B.cons` l'
        (a', n) = B.breakEnd dollarPred a
        (n', x) = B.break commaPred n
        num     = B.pack $ show $ readInt n' + spFix
        -- We need to avoid processing jumps to labels, they are of the form:
        -- jmp\tL..., jmp\t_f..., jmpl\t_f..., jmpl\t*%eax..., jmpl *L...
        targ = B.dropWhile ((==)'*') $ B.drop 1 $ B.dropWhile ((/=)'\t') $
                B.drop labelStart c
    in if B.null c
          then f' `B.append` f
          else if B.head targ == 'L'
                then fixupStack b $ f' `B.append` a `B.append` l
                else fixupStack b $ f' `B.append` a' `B.append` num `B.append`
                                    x `B.append` l

-- | read an int or error
readInt :: B.ByteString -> Int
readInt str | B.all isDigit str = (read . B.unpack) str
            | otherwise = error $ "LLvmMangler Cannot read" ++ show str
                                ++ "as it's not an Int"

