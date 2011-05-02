import System.Environment
import System.FilePath

import System.IO
import qualified Data.ByteString.Char8 as B

import Criterion.Main

type CopyFun = Maybe String -> FilePath -> FilePath -> IO ()

copy :: CopyFun
copy mHeader from to = do
    hout <- openBinaryFile to   WriteMode
    hin  <- openBinaryFile from ReadMode
    file <- hGetContents hin
    maybe (return ()) (hPutStr hout) mHeader
    hPutStr hout file
    hClose hout
    hClose hin

copyB :: CopyFun
copyB mHeader from to = do
    hout <- openBinaryFile to   WriteMode
    hin  <- openBinaryFile from ReadMode
    file <- B.hGetContents hin
    maybe (return ()) ((B.hPut hout) . B.pack) mHeader
    B.hPut hout file
    hClose hout
    hClose hin

copyFiles :: CopyFun -> Maybe String -> [String] -> IO ()
copyFiles cpFun header files = do
    let ofiles = map (flip addExtension ".copy") files
    mapM_ (\(x,y) -> cpFun header x y) $ zip files ofiles

main = do
    let small = ("[Small]", ["Copy.hs", "Copy", "Copy.o", "Copy.hi"])
        med   = ("[Medium]", ["Med", "Med2", "Med3"])
        big   = ("[Big]", ["Big", "Big2"])
        cp    = ("copy (System.IO)", copyFiles copy)
        cpB   = ("copy (Data.ByteString)", copyFiles copyB)
        hdr   = Just "==== HEADER ===="

    let bench' (cpS, cpF) (fileS, fileSet)
            = [bench (cpS ++ fileS) $ nfIO $ cpF Nothing fileSet
              ,bench (cpS ++ "[header]" ++ fileS) $ nfIO $ cpF hdr fileSet]

    defaultMain $ concat
        [ bench' cp small
        , bench' cpB small
        , bench' cp  med
        , bench' cpB med
        , bench' cp  big
        , bench' cpB big
        ]

    return ()

