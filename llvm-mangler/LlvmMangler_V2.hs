{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- -----------------------------------------------------------------------------
-- | GHC LLVM Mangler
--
-- This script processes the assembly produced by LLVM, rearranging the code
-- so that an info table appears before its corresponding function. We also
-- use it to fix up the stack alignment, which needs to be 16 byte aligned
-- but always ends up off by 4 bytes because GHC sets it to the wrong starting
-- value in the RTS.
--
-- We only need this for Mac OS X, other targets don't use it.
--

module LlvmMangler_V2 ( llvmFixupAsm ) where

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Unsafe ( PS )
import System.IO

-- import LlvmCodeGen.Ppr ( infoSection, iTableSuf )

import Data.Char
-- import Outputable
-- import Util

-- infoSection = ".text; .text "
infoSection = "__STRIP,__me"
iTableSuf = "_itable"
ghciTablesNextToCode = True

{- Configuration. -}
newSection, oldSection, functionSuf, tableSuf, funDivider, eol :: ByteString
newSection  = BS.pack "\n.text\n"
oldSection  = BS.pack infoSection
functionSuf = BS.pack $ if ghciTablesNextToCode then "_info:" else "\n_"
tableSuf    = BS.pack $ "_info" ++ iTableSuf ++ ":"
funDivider  = BS.pack "\n\n"
eol         = BS.pack "\n"
dataStart   = BS.pack ".data\n"


eolPred, dollarPred, commaPred :: Char -> Bool
eolPred = ((==) '\n')
dollarPred = ((==) '$')
commaPred = ((==) ',')

-- | Read in assembly file and process
llvmFixupAsm :: FilePath -> FilePath -> IO ()
llvmFixupAsm f1 f2 = do
    asm <- BS.readFile f1
    BS.writeFile f2 BS.empty
    h <- openBinaryFile f2 AppendMode
    allTables h (breakSubstring dataStart asm)
    return ()

-- | Run over whole assembly file
allTables :: Handle -> (ByteString, ByteString) -> IO ()
allTables h str = do
    (funcs, globs) <- oneTable h str
    if BS.null funcs
       then do
           BS.hPut h globs
           return ()
       else allTables h (funcs, globs)

{- |
  Look for the next function that needs to have its info table
  arranged to be before it and process it. This will print out
  any code before this function, then the info table, then the
  function. It will return the remainder of the assembly code
  to process.

  We rely here on the fact that LLVM prints all global variables
  at the end of the file, so an info table will always appear
  after its function.

  To try to help explain the string searches, here is some
  assembly code that would be processed by this program, with
  split markers placed in it like so, <split marker>:

    [ ...asm code... ]
    jmp *%eax
    <before|fheader>
    .def Main_main_info
    .section TEXT
    .globl _Main_main_info
    _Main_main<bl|al>_info:
        sub $12, %esp
        [ ...asm code... ]
        jmp *%eax
    <fun|after>
    .def .....

    [ ...asm code... ]

        .long 231231
    <bit'|itable_h>
    .section TEXT
    .global _Main_main_entry
    .align 4
    <bit|itable>_Main_main_info_itable:
        .long 0
        [ ...asm code... ]
    <itable'|ait>
    .section TEXT
-}
oneTable :: Handle -> (ByteString, ByteString) -> IO (ByteString, ByteString)
oneTable h (fStr, iStr) =
    let last' xs = if (null xs) then 0 else last xs

        -- get the function
        (bl, al)          = breakSubstring functionSuf fStr
        start             = last' $ findSubstrings funDivider bl
        (before, fheader) = BS.splitAt start bl
        (fun, after)      = breakSubstring funDivider al
        label             = snd $ BS.breakEnd eolPred bl

        -- get the info table
        ilabel            = label `BS.append` tableSuf
        (bit, itable)     = breakSubstring ilabel iStr
        (itable', ait)    = breakSubstring funDivider itable
        istart            = last' $ findSubstrings funDivider bit
        (bit', iheader)   = BS.splitAt istart bit

        -- fixup stack alignment
        fun' = fixupStack fun BS.empty

        -- fix up sections
        fheader' = replaceSection fheader
        iheader' = replaceSection iheader

        function = [before, eol, iheader', itable', eol, fheader', fun', eol]
        irem     = bit' `BS.append` ait
    in if BS.null al
          then do
              BS.hPut h fStr
              BS.hPut h iStr
              return (BS.empty, BS.empty)

          else if ghciTablesNextToCode
                  then if BS.null itable
                          then error $ "Function without matching info table! ("
                                      ++ (BS.unpack label) ++ ")"
                          else do
                              BS.hPut h $ BS.concat function
                              return (after, irem)

                  else do
                      -- TNTC not turned on so just fix up stack
                      BS.hPut h $ BS.concat [before, fheader, fun']
                      return (after, BS.empty)

-- | Replace the current section in a function or table header with the
-- text section specifier.
replaceSection :: ByteString -> ByteString
replaceSection sec =
    let (s1, s2) = breakSubstring oldSection sec
        s1'      = fst $ BS.breakEnd eolPred s1
        s2'      = snd $ BS.break eolPred s2
    in s1' `BS.append` newSection `BS.append` s2'


-- | Mac OS X requires that the stack be 16 byte aligned when making a function
-- call (only really required though when making a call that will pass through
-- the dynamic linker). During code generation we marked any points where we
-- make a call that requires this alignment. The alignment isn't correctly
-- generated by LLVM as LLVM rightly assumes that the stack wil be aligned to
-- 16n + 12 on entry (since the function call was 16 byte aligned and the return
-- address should have been pushed, so sub 4). GHC though since it always uses
-- jumps keeps the stack 16 byte aligned on both function calls and function
-- entry. We correct LLVM's alignment then by putting inline assembly in that
-- subtracts and adds 4 to the sp as required.
fixupStack :: ByteString -> ByteString -> ByteString
fixupStack fun nfun | BS.null nfun =
    let -- fixup sub op
        (a, b)       = breakSubstring (BS.pack ", %esp\n") fun
        (a', strNum) = BS.breakEnd dollarPred a
        Just num     = readInt (BS.unpack strNum)
        num'         = BS.pack $ show (num + 4::Int)
        fix          = a' `BS.append` num'
    in if BS.null b
          then nfun `BS.append` a
          else fixupStack b (nfun `BS.append` fix)

fixupStack fun nfun =
    let -- fixup add ops
        (a, b) = breakSubstring (BS.pack "jmp") fun
        -- We need to avoid processing jumps to labels, they are of the form:
        -- jmp\tL..., jmp\t_f..., jmpl\t_f..., jmpl\t*%eax...
        labelJump   = BS.index b 4 == 'L'
        (jmp, b')   = BS.break eolPred b
        (a', numx)  = BS.breakEnd dollarPred a
        (strNum, x) = BS.break commaPred numx
        Just num    = readInt (BS.unpack strNum)
        num'        = BS.pack $ show (num + 4::Int)
        fix         = a' `BS.append` num' `BS.append` x `BS.append` jmp
    in if BS.null b
          then nfun `BS.append` a
          else if labelJump
                then fixupStack b' (nfun `BS.append` a `BS.append` jmp)
                else fixupStack b' (nfun `BS.append` fix)


-- | 'read' is one of my least favourite functions.
readInt :: String -> Maybe Int
readInt str
	| not $ null $ filter (not . isDigit) str
	= error $ "LLvmMangler" ++
		"Cannot read" ++ show str ++ "as it's not an Int"

	| otherwise
	= Just $ read str

breakSubstring :: BS.ByteString -- ^ String to search for
               -> BS.ByteString -- ^ String to search in
               -> (BS.ByteString,BS.ByteString) -- ^ Head and tail of string broken at substring

breakSubstring pat src = search 0 src
  where
    search n s
        | null s             = (src,BS.empty)      -- not found
        | pat `BS.isPrefixOf` s = (BS.take n src,s)
        | otherwise          = search (n+1) (unsafeTail s)

findSubstring :: BS.ByteString -- ^ String to search for.
              -> BS.ByteString -- ^ String to seach in.
              -> Maybe Int
findSubstring f i = listToMaybe (findSubstrings f i)

findSubstrings :: BS.ByteString -- ^ String to search for.
               -> BS.ByteString -- ^ String to seach in.
               -> [Int]
findSubstrings pat str
    | null pat         = [0 .. length str]
    | otherwise        = search 0 str
  where
    search n s
        | null s             = []
        | pat `BS.isPrefixOf` s = n : search (n+1) (unsafeTail s)
        | otherwise          =     search (n+1) (unsafeTail s)

breakEnd :: (Char -> Bool) -> BS.ByteString -> (BS.ByteString, BS.ByteString)
breakEnd  p ps = BS.splitAt (BS.findFromEndUntil p ps) ps

findFromEndUntil :: (Char -> Bool) -> BS.ByteString -> Int
findFromEndUntil f ps@(PS x s l) =
    if null ps then 0
    else if f (last ps) then l
         else findFromEndUntil f (PS x s (l-1))
