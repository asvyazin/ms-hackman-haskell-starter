module Main where


import Codec.Archive.Zip (readEntry, Entry(eRelativePath), emptyArchive, addEntryToArchive, fromArchive)
import qualified Data.ByteString.Lazy as BL (writeFile)
import Data.List (find, foldl')
import qualified Distribution.ModuleName as M (main)
import Distribution.PackageDescription (PackageDescription(..), Executable(..), BuildInfo(..), exeModules)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (findModuleFiles)
import Distribution.Verbosity (verbose)
import System.FilePath ((</>))


cabalPath :: String
cabalPath = "ms-hackman.cabal"


executableName :: String
executableName = "ms-hackman-exe"


zipFileName :: FilePath
zipFileName = "bot.zip"


main :: IO ()
main = do
  genDesc <- readPackageDescription verbose cabalPath
  let desc = flattenPackageDescription genDesc
  case findExe desc executableName of
    Just exe -> do
      hsFiles <- getHsFiles exe
      let additionalFiles = map (\x -> (".", x)) $ licenseFiles desc ++ extraSrcFiles desc
          files = hsFiles ++ additionalFiles
      print files
      entries <- mapM getEntry files
      let archive = foldl' (flip addEntryToArchive) emptyArchive entries
          archiveContents = fromArchive archive
      BL.writeFile zipFileName archiveContents
      putStrLn $ zipFileName ++ " written"
    _ -> error $ "Executable " ++ executableName ++ " not found in " ++ cabalPath


findExe :: PackageDescription -> String -> Maybe Executable
findExe desc name =
  find (\e -> exeName e == name) $ executables desc


getHsFiles :: Executable -> IO [(FilePath, FilePath)]
getHsFiles exe = do
  let sourceDirs = hsSourceDirs $ buildInfo exe
  findModuleFiles sourceDirs [".hs"] (M.main : exeModules exe)


getEntry :: (FilePath, FilePath) -> IO Entry
getEntry (dir, filename) = do
  e <- readEntry [] $ dir </> filename
  pure $ e { eRelativePath = filename }
