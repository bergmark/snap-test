{-# LANGUAGE ViewPatterns #-}
module BuildFay (buildFay) where

import           Control.Applicative
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.Process

-- import qualified Language.Fay.Compiler as Fay

-- | Configuration

srcDir :: FilePath
srcDir = "/Users/adam/repos/snap-test/src/Fay"

destDir :: FilePath
destDir = "/Users/adam/repos/snap-test/js"

-- | Generic Helpers

hasPrefix :: String -> String -> Bool
hasPrefix s prefix = prefix == take (length prefix) s

hasSuffix :: String -> String -> Bool
hasSuffix s suffix = reverse suffix == take (length suffix) (reverse s)

filename :: FilePath -> FilePath
filename = reverse . takeWhile (/= '/') . reverse

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x

-- | Build

fayFiles :: FilePath -> IO [FilePath]
fayFiles dir = filter (`hasSuffix` ".hs") <$> getDirectoryContents dir

buildFay :: IO ()
buildFay = do
  putStrLn "buildFay"
  files <- fayFiles srcDir
  print files
  forM_ files $ \f -> do
             putStrLn $ "compiling " ++ f ++ " -> " ++ (destDir </> filename (toJsName f))
             system $ "fay -autorun \"" ++ f ++ "\""
