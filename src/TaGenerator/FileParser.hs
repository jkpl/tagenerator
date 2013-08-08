module TaGenerator.FileParser (readTextAdventureFromDirectory) where

import Data.Monoid
import System.IO
import System.Directory
import System.FilePath
import TaGenerator.TaData


readTextAdventureFromDirectory :: FilePath -> IO (Maybe TextAdventure)
readTextAdventureFromDirectory fpath = do
    dircontents <- getDirectoryFilePaths fpath
    readAllTaFiles $ textAdventureFiles dircontents

getDirectoryFilePaths :: FilePath -> IO [FilePath]
getDirectoryFilePaths path = do
    dircontents <- getDirectoryContents path
    return $ fmap (combine path) dircontents

readAllTaFiles :: [FilePath] -> IO (Maybe TextAdventure)
readAllTaFiles files = fmap mconcat $ mapM readTaFromFile files

readTaFromFile :: FilePath -> IO (Maybe TextAdventure)
readTaFromFile fname = do
    handle <- openFile fname ReadMode
    contents <- hGetContents handle
    return $ parseTextAdventure contents

textAdventureFiles :: [FilePath] -> [FilePath]
textAdventureFiles = filter ((==) ".ta" . takeExtension)
