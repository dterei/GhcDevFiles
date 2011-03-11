{-# LANGUAGE BangPatterns #-}
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

module LlvmMangler_V3 ( llvmFixupAsm ) where

import qualified Prelude as P
import Prelude hiding ( splitAt, concat, null, break, readFile, writeFile, filter )
import Data.ByteString.Char8 ( ByteString, pack, append, empty, splitAt, breakEnd,
                               readFile, writeFile, null, concat, hPut, index,
                               unpack, break, filter )
import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeIndex)
import System.IO ( IO, openBinaryFile, IOMode(..), Handle )
import Data.Word (Word8)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.ST hiding ( index )
import Data.Array.Unboxed hiding ( index )

-- import LlvmCodeGen.Ppr ( infoSection, iTableSuf )

import Data.Char
-- import Outputable
-- import Util

-- infoSection = ".text; .text "
infoSection = "__STRIP,__me"
iTableSuf = "_itable"
ghciTablesNextToCode = True

{- Configuration. -}
newSection, tableSuf, eol :: ByteString
newSection  = pack "\n.text\n"
tableSuf    = pack $ "_info" ++ iTableSuf ++ ":"
eol         = pack "\n"

sOldSection, sFunctionSuf, sFunDivider, sDataStart :: ByteString -> (ByteString, ByteString)
sOldSection  = breakOn $ pack infoSection
sFunctionSuf = breakOn $ pack $ if ghciTablesNextToCode then "_info:" else "\n_"
sFunDivider  = breakOn $ pack "\n\n"
sDataStart   = breakOn $ pack ".data\n"

sFunDividerI :: ByteString -> [Int]
sFunDividerI = nonOverlappingIndices $ pack "\n\n"

eolPred, dollarPred, commaPred :: Char -> Bool
eolPred = ((==) '\n')
dollarPred = ((==) '$')
commaPred = ((==) ',')

-- | Read in assembly file and process
llvmFixupAsm :: FilePath -> FilePath -> IO ()
llvmFixupAsm f1 f2 = do
    asm <- readFile f1
    writeFile f2 empty
    h <- openBinaryFile f2 AppendMode
    allTables h (sDataStart asm)
    return ()

-- | Run over whole assembly file
allTables :: Handle -> (ByteString, ByteString) -> IO ()
allTables h str = do
    (funcs, globs) <- oneTable h str
    if null funcs
       then do
           hPut h globs
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
    <bit|itable>_Main_main_entry:
        .long 0
        [ ...asm code... ]
    <itable'|ait>
    .section TEXT
-}
oneTable :: Handle -> (ByteString, ByteString) -> IO (ByteString, ByteString)
oneTable h (fStr, iStr) =
    let -- get the function
        (bl, al)          = sFunctionSuf fStr
        start             = last' $ sFunDividerI bl
        (before, fheader) = splitAt start bl
        (fun, after)      = sFunDivider al
        label             = snd $ breakEnd eolPred bl

        -- get the info table
        ilabel            = label `append` tableSuf
        (bit, itable)     = breakOn ilabel iStr
        (itable', ait)    = sFunDivider itable
        istart            = last' $ sFunDividerI bit
        (bit', iheader)   = splitAt istart bit

        -- fixup stack alignment
        fun' = fixupStack fun empty

        -- fix up sections
        fheader' = replaceSection fheader
        iheader' = replaceSection iheader

        function = [before, eol, iheader', itable', eol, fheader', fun', eol]
        irem     = bit' `append` ait
    in if null al
          then do
              hPut h fStr
              hPut h iStr
              return (empty, empty)

          else if ghciTablesNextToCode
                  then if null itable
                          then error $ "Function without matching info table! ("
                                      ++ (unpack label) ++ ")"
                          else do
                              hPut h $ concat function
                              return (after, irem)

                  else do
                      -- TNTC not turned on so just fix up stack
                      hPut h $ concat [before, fheader, fun']
                      return (after, empty)

-- | Replace the current section in a function or table header with the
-- text section specifier.
replaceSection :: ByteString -> ByteString
replaceSection sec =
    let (s1, s2) = sOldSection sec
        s1'      = fst $ breakEnd eolPred s1
        s2'      = snd $ break eolPred s2
    in s1' `append` newSection `append` s2'

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
fixupStack fun nfun | null nfun =
    let -- fixup sub op
        (a, b)       = breakOn (pack ", %esp\n") fun
        (a', strNum) = breakEnd dollarPred a
        Just num     = readInt (unpack strNum)
        num'         = pack $ show (num + 4::Int)
        fix          = a' `append` num'
    in if null b
          then nfun `append` a
          else fixupStack b (nfun `append` fix)

fixupStack fun nfun =
    let -- fixup add ops
        (a, b) = breakOn (pack "jmp") fun
        -- We need to avoid processing jumps to labels, they are of the form:
        -- jmp\tL..., jmp\t_f..., jmpl\t_f..., jmpl\t*%eax...
        labelJump   = index b 4 == 'L'
        (jmp, b')   = break eolPred b
        (a', numx)  = breakEnd dollarPred a
        (strNum, x) = break commaPred numx
        Just num    = readInt (unpack strNum)
        num'        = pack $ show (num + 4::Int)
        fix         = a' `append` num' `append` x `append` jmp
    in if null b
          then nfun `append` a
          else if labelJump
                then fixupStack b' (nfun `append` a `append` jmp)
                else fixupStack b' (nfun `append` fix)

-- | Read an Int Safely
readInt :: String -> Maybe Int
readInt str
	| P.not $ P.null $ P.filter (not . isDigit) str
	= error $ "LLvmMangler" ++
		"Cannot read" ++ show str ++ "as it's not an Int"

	| otherwise
	= Just $ read str

-- | Safe version of last
last' :: [Int] -> Int
last' xs = if (P.null xs) then 0 else P.last xs

-- | @nonOverlappingIndices@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Pattern to find
                      -> S.ByteString   -- ^ String to search
                      -> [Int]          -- ^ Offsets of matches
nonOverlappingIndices = strictSearcher False

-- | @breakOn pattern target@ splits @target@ at the first occurrence
--   of @pattern@. If the pattern does not occur in the target, the
--   second component of the result is empty, otherwise it starts with
--   @pattern@. If the pattern is empty, the first component is empty.
--
-- @
--   'uncurry' 'S.append' . 'breakOn' pattern = 'id'
-- @
{-# INLINE breakOn #-}
breakOn :: S.ByteString  -- ^ String to search for
        -> S.ByteString  -- ^ String to search in
        -> (S.ByteString, S.ByteString)
                         -- ^ Head and tail of string broken at substring
breakOn pat
    | S.null pat    = \str -> (S.empty, str)
    | otherwise     = breaker
      where
        searcher = strictSearcher False pat
        breaker str = case searcher str of
                        []      -> (str, S.empty)
                        (i:_)   -> S.splitAt i str



strictSearcher :: Bool -> S.ByteString -> S.ByteString -> [Int]
strictSearcher _ !pat
    | S.null pat = enumFromTo 0 . S.length
    | S.length pat == 1 = let !w = S.head pat in S.elemIndices w
strictSearcher !overlap pat = searcher
  where
    {-# INLINE patAt #-}
    patAt :: Int -> Word8
    patAt !i = unsafeIndex pat i

    !patLen = S.length pat
    !patEnd = patLen - 1
    !maxLen = maxBound - patLen
    !occT   = occurs pat        -- for bad-character-shift
    !suffT  = suffShifts pat    -- for good-suffix-shift
    !skip   = if overlap then unsafeAt suffT 0 else patLen
    -- shift after a complete match
    !kept   = patLen - skip     -- length of known prefix after full match
    !pe     = patAt patEnd      -- last pattern byte for fast comparison

    {-# INLINE occ #-}
    occ !w = unsafeAt occT (fromIntegral w)

    {-# INLINE suff #-}
    suff !i = unsafeAt suffT i

    searcher str
        | maxLen < strLen
            = error "Overflow in BoyerMoore.strictSearcher"
        | maxDiff < 0   = []
        | otherwise     = checkEnd patEnd
          where
            !strLen  = S.length str
            !strEnd  = strLen - 1
            !maxDiff = strLen - patLen

            {-# INLINE strAt #-}
            strAt !i = unsafeIndex str i

            -- After a full match, we know how long a prefix of the pattern
            -- still matches. Do not re-compare the prefix to prevent O(m*n)
            -- behaviour for periodic patterns.
            afterMatch !diff !patI =
              case strAt (diff + patI) of
                !c  | c == patAt patI ->
                      if patI == kept
                        then diff : let !diff' = diff + skip
                                    in if maxDiff < diff'
                                        then []
                                        else afterMatch diff' patEnd
                        else afterMatch diff (patI - 1)
                    | patI == patEnd  ->
                            checkEnd (diff + 2*patEnd + occ c)
                    | otherwise       ->
                            let {-# INLINE badShift #-}
                                badShift = patI + occ c
                                {-# INLINE goodShift #-}
                                goodShift = suff patI
                                !diff' = diff + max badShift goodShift
                            in if maxDiff < diff'
                                then []
                                else checkEnd (diff + patEnd)

            -- While comparing the last byte of the pattern, the bad-
            -- character-shift is always at least as large as the good-
            -- suffix-shift. Eliminating the unnecessary memory reads and
            -- comparison speeds things up noticeably.
            checkEnd !sI  -- index in string to compare to last of pattern
                | strEnd < sI   = []
                | otherwise     =
                  case strAt sI of
                    !c  | c == pe   -> findMatch (sI - patEnd) (patEnd - 1)
                        | otherwise -> checkEnd (sI + patEnd + occ c)

            -- Once the last byte has matched, we enter the full matcher
            -- diff is the offset of the window, patI the index of the
            -- pattern byte to compare next.
            findMatch !diff !patI =
                case strAt (diff + patI) of
                    !c  | c == patAt patI ->
                            if patI == 0    -- full match, report
                                then diff : let !diff' = diff + skip
                                            in if maxDiff < diff'
                                                then []
                                                else
                                                  if skip == patLen
                                                    then
                                                      checkEnd (diff' + patEnd)
                                                    else
                                                      afterMatch diff' patEnd
                                else findMatch diff (patI - 1)
                        | otherwise       ->
                            let !diff' = diff + max (patI + occ c) (suff patI)
                            in if maxDiff < diff'
                                then []
                                else checkEnd (diff' + patEnd)

{-# INLINE occurs #-}
occurs :: S.ByteString -> UArray Int Int
occurs pat = runSTUArray (do
    let !patEnd = S.length pat - 1
        {-# INLINE patAt #-}
        patAt :: Int -> Int
        patAt i = fromIntegral (unsafeIndex pat i)
    ar <- newArray (0, 255) 1
    let loop !i
            | i == patEnd   = return ar
            | otherwise     = do
                unsafeWrite ar (patAt i) (-i)
                loop (i + 1)
    loop 0)

{-# INLINE suffShifts #-}
suffShifts :: S.ByteString -> UArray Int Int
suffShifts pat = runSTUArray (do
    let !patLen = S.length pat
        !patEnd = patLen - 1
        !suff   = suffLengths pat
    ar <- newArray (0,patEnd) patLen
    let preShift !idx !j
            | idx < 0   = return ()
            | suff `unsafeAt` idx == idx + 1 = do
                let !shf = patEnd - idx
                    fillToShf !i
                        | i == shf  = return ()
                        | otherwise = do
                            unsafeWrite ar i shf
                            fillToShf (i + 1)
                fillToShf j
                preShift (idx - 1) shf
            | otherwise = preShift (idx - 1) j
        sufShift !idx
            | idx == patEnd = return ar
            | otherwise     = do
                unsafeWrite ar (patEnd - unsafeAt suff idx) (patEnd - idx)
                sufShift (idx + 1)
    preShift (patEnd - 1) 0
    sufShift 0)

{-# INLINE suffLengths #-}
suffLengths :: S.ByteString -> UArray Int Int
suffLengths pat = runSTUArray (do
    let !patLen = S.length pat
        !patEnd = patLen - 1
        !preEnd = patEnd - 1
        {-# INLINE patAt #-}
        patAt i = unsafeIndex pat i
        -- last byte for comparisons
        !pe     = patAt patEnd
        -- find index preceding the longest suffix
        dec !diff !j
            | j < 0 || patAt j /= patAt (j + diff) = j
            | otherwise = dec diff (j - 1)
    ar <- newArray_ (0, patEnd)
    unsafeWrite ar patEnd patLen
    let noSuff !i
            | i < 0     = return ar
            | patAt i == pe = do
                let !diff  = patEnd - i
                    !nextI = i - 1
                    !prevI = dec diff nextI
                if prevI == nextI
                    then unsafeWrite ar i 1 >> noSuff nextI
                    else do unsafeWrite ar i (i - prevI)
                            suffLoop prevI preEnd nextI
            | otherwise = do
                unsafeWrite ar i 0
                noSuff (i - 1)
        suffLoop !pre !end !idx
            | idx < 0   = return ar
            | pre < idx =
              if patAt idx /= pe
                then unsafeWrite ar idx 0 >> suffLoop pre (end - 1) (idx - 1)
                else do
                    prevS <- unsafeRead ar end
                    if pre + prevS < idx
                        then do unsafeWrite ar idx prevS
                                suffLoop pre (end - 1) (idx - 1)
                        else do let !prI = dec (patEnd - idx) pre
                                unsafeWrite ar idx (idx - prI)
                                suffLoop prI preEnd (idx - 1)
            | otherwise = noSuff idx
    noSuff preEnd)

