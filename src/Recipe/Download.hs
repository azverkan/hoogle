{-# LANGUAGE CPP #-}
module Recipe.Download(download) where

import General.Base
import General.System
import Recipe.Type

#if FLAG_downloader
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Browser
import Network.HTTP
#endif

type Downloader = FilePath -> URL -> IO Bool

wget :: FilePath -> URL -> String
wget fp url = "wget -nv " ++ url ++ " --output-document=" ++ fp
curl :: FilePath -> URL -> String
curl fp url = "curl -sSL " ++ url ++ " --output " ++ fp

findExternalDownloader :: IO Downloader
findExternalDownloader = do
    dl <- liftM2 mplus (check "wget") (check "curl")
    when (isNothing dl) $ error "Could not find downloader, neither curl nor wget are on the $PATH."
    return $ \fp url -> sys $ matchDl (fromJust dl) fp url
    where matchDl d | "wget" `isInfixOf` d = wget
                    | "curl" `isInfixOf` d = curl
          sys = fmap (== ExitSuccess) . system

#ifdef FLAG_downloader
httpGet :: Downloader
httpGet fp url = do
    rsp <- liftM snd $ browse $ request $ asLazy $ getRequest url
    LBS.writeFile fp $ rspBody rsp
    return True
    where asLazy r = r { rqBody = LBS.empty }
#endif

findDownloader :: IO Downloader
#ifdef FLAG_downloader
findDownloader = return httpGet
#else
findDownloader = findExternalDownloader
#endif

withDownloader :: CmdLine -> Downloader -> [(FilePath, FilePath, URL)] -> IO ()
withDownloader opt downloader items =
    let download (f, f', u) = do
            b <- doesFileExist f
            when (not b || redownload opt) $ do
                res <- downloader f' u
                unless res $ do
                    b <- doesFileExist f'
                    when b $ removeFile f'
                    error $ "Failed to download: " ++ u
            doesFileExist f'
    in forM_ items download

-- download everything required for the recipes
download :: CmdLine -> IO ()
download opt = do
    createDirectoryIfMissing True "download"
    downloader <- findDownloader
    let items = [ (keywords, keywords, "http://www.haskell.org/haskellwiki/Keywords")
                , (platform, platform, "http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal")
                , ("download/base.txt", "download/base.txt", "http://www.haskell.org/hoogle/base.txt")
                , ("download/ghc.txt",  "download/ghc.txt", "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/ghc.txt")
                , (cabals <.> "txt", cabals <.> "tar.gz", "http://hackage.haskell.org/packages/archive/00-index.tar.gz")
                , (inputs <.> "txt", inputs <.> "tar.gz", "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz")
                ]
    withDownloader opt downloader items
    extractTarball cabals
    extractTarball inputs


check :: String -> IO (Maybe FilePath)
check name = do
    res <- findExecutable name
    when (isNothing res) $ do
        putStrLn $ "WARNING: Could not find command line program " ++ name ++ "."
        when isWindows $ putStrLn $ "  You may be able to install it from:\n  " ++ url
    return res
    where
        srcList = [ ("gzip", "http://gnuwin32.sourceforge.net/packages/gzip.htm")
                  , ("tar",  "http://gnuwin32.sourceforge.net/packages/gtar.htm")
                  , ("wget", "http://gnuwin32.sourceforge.net/packages/wget.htm")
                  , ("curl", "http://curl.haxx.se/download.html")
                  ]
        url = fromJust . lookup name $ srcList

extractTarball :: FilePath -> IO ()
extractTarball out = do
        createDirectoryIfMissing True out
        withDirectory out $ do
#ifdef FLAG_downloader
            putStrLn "Extracting tarball... "
            Tar.unpack "." . Tar.read . GZip.decompress =<< LBS.readFile (".." </> takeFileName out <.> "tar.gz")
#else
            hasGzip <- check "gzip"
            hasTar  <- check "tar"
            when (any isNothing [hasGzip, hasTar]) $ error "Could not extract tarball(s), could not find either gzip or tar on the $PATH."
            putStrLn "Extracting tarball... "
            system_ $ "gzip --decompress --force .." </> takeFileName out <.> "tar.gz"
            system_ $ "tar -xf .." </> takeFileName out <.> "tar"
#endif
            putStrLn "Finished extracting tarball"
        writeFile (out <.> "txt") ""
