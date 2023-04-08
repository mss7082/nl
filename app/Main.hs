module Main where

import System.Environment


printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe (printHelpText "Missing filename") (\filePath -> putStrLn filePath) mFilePath

parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing


readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)

