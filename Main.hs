{-#LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TupleSections#-}
import Control.Applicative
import Control.Monad

import System.Directory
import System.FilePath

import Data.String

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem

import qualified Filesystem.Path       as FS
import qualified Filesystem.Path.Rules as FS

import Codec.Compression.GZip
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as BL

base :: IO FilePath
base = getHomeDirectory >>= \home -> return $ home </> ".cabal/packages/cabal-src/"

gzipOnly :: Monad m => Conduit FS.FilePath m FS.FilePath
gzipOnly = CL.filter (\e -> let es = FS.extensions e
                            in last es == "gz" && last (init es) == "tar"
                     )

getCabal :: FilePath -> IO (Maybe BL.ByteString)
getCabal path = BL.readFile path >>= return. Tar.foldEntries folding Nothing (const Nothing). Tar.read. decompress
  where folding e a = if FS.extension (fromString $ Tar.entryPath e) == Just "cabal"
                      then case Tar.entryContent e of
                        Tar.NormalFile s _ -> Just s
                        _                  -> a
                      else a

main :: IO ()
main = do fp <- base
          es <- Tar.write <$> toEntries fp (FS.encodeString FS.darwin)
          BL.writeFile (fp </> "00-index.tar") es
          let cache =  fp </> "00-index.cache"
          doesFileExist cache >>= \e -> when e (removeFile cache)

cabalFileNameAndContent :: String -> (FS.FilePath -> FilePath)
                -> Conduit FS.FilePath IO (Maybe (FS.FilePath, BL.ByteString))
cabalFileNameAndContent fp encode = CL.mapM (\e -> (getCabal $ encode e) >>= \r -> case r of
                                                Just c  -> return $ (,c) <$> toCabalFN e
                                                Nothing -> return Nothing
                                            )
  where toCabalFN = FS.stripPrefix (fromString fp) .
                    flip FS.replaceExtension "cabal" . FS.dropExtension

toEntries :: String -> (FS.FilePath -> FilePath) -> IO [Tar.Entry]
toEntries fp encode =
  traverse False (fromString fp) $$ gzipOnly =$
  cabalFileNameAndContent fp encode =$
  CL.catMaybes =$
  CL.mapM (\a@(e,_) -> putStrLn (encode e) >> return a) =$
  CL.map (\(p,e) -> case Tar.toTarPath False $ encode p
                      of Left _  -> Nothing
                         Right r -> Just (r, e)
         ) =$
  CL.catMaybes =$
  CL.map (uncurry Tar.fileEntry) =$
  CL.consume

