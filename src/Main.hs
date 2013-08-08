module Main where

import Control.Monad.Writer (runWriter)
import System.IO
import System.Environment (getArgs)
import System.FilePath (combine)
import TaGenerator.TaData (TextAdventure)
import TaGenerator.TaCheck
import TaGenerator.FileParser
import TaGenerator.GameEngine


main :: IO ()
main = do
    args <- getArgs
    let fpath = targetPath args
    putStrLn $ "Reading text adventure from path: " ++ fpath
    perhapsTa <- readTextAdventureFromDirectory fpath
    case perhapsTa of
        Just ta -> checkTaAndPlay ta
        Nothing -> putStrLn "Failed to parse game."

targetPath :: [String] -> FilePath
targetPath (x:_) = combine "." x
targetPath [] = "."

checkTaAndPlay :: TextAdventure -> IO ()
checkTaAndPlay ta = do
    let warnings = runChecker ta
    case null warnings of
        True -> playGame ta
        False -> printWarnings warnings

playGame :: TextAdventure -> IO ()
playGame ta = startGameLoop ta lineReader

lineReader :: IO String
lineReader = do
    putStr ">> "
    hFlush stdout
    getLine

printWarnings :: [String] -> IO ()
printWarnings [] = putStrLn "No warnings found."
printWarnings xs = mapM_ putStrLn ["Found warnings:", showWarningsAsList xs]

showWarningsAsList :: [String] -> String
showWarningsAsList = concatMap render
  where render s = concat ["- ", s, "\n"]
