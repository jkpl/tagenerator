module Main where

import Data.Monoid
import Control.Monad.Writer (runWriter)
import System.IO
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import TaGenerator.TaData
import TaGenerator.TaCheck


main :: IO ()
main = do
    args <- getArgs
    dircontents <- getDirectoryFilePaths $ targetPath args
    tas <- readAllTaFiles $ textAdventureFiles dircontents
    print $ fmap (runWriter . checkTextAdventure) tas

getDirectoryFilePaths :: FilePath -> IO [FilePath]
getDirectoryFilePaths path = do
    dircontents <- getDirectoryContents path
    return $ fmap (combine path) dircontents

targetPath :: [String] -> FilePath
targetPath (x:_) = combine "." x
targetPath [] = "."

textAdventureFiles :: [FilePath] -> [FilePath]
textAdventureFiles = filter ((==) ".ta" . takeExtension)

readAllTaFiles :: [FilePath] -> IO (Maybe TextAdventure)
readAllTaFiles files = fmap mconcat $ mapM readTaFromFile files

readTaFromFile :: FilePath -> IO (Maybe TextAdventure)
readTaFromFile fname = do
    handle <- openFile fname ReadMode
    contents <- hGetContents handle
    return $ parseTextAdventure contents
