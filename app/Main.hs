{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Time (defaultTimeLocale, getCurrentTime, parseTimeM)
import Data.Time.Format (formatTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

dir = "/Users/sibin/Documents/test"

main :: IO ()
main =
  getArgs >>= \case
    [name, cmd] -> parseCmd cmd name
    [name, cmd, opt] -> do
      parseCmd cmd name
      putStrLn opt
    _ -> do
      hPutStrLn stderr "Invalid arguments"
      exitWith (ExitFailure 1)

parseCmd :: String -> String -> IO ()
parseCmd cmd name = do
  case cmd of
    "inc" -> inc (fileName name)
    "dec" -> putStrLn "Imma dec that for ya"
    "ls" -> readFromFile (fileName name) >>= mapM_ putStrLn
    "c" -> putStrLn "Imma count that for ya"
    _ -> do
      hPutStrLn stderr "Invalid command"
      exitWith (ExitFailure 1)

inc :: String -> IO ()
inc file = do
  now <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  writeToFile file formattedTime

writeToFile :: String -> String -> IO ()
writeToFile file date = do
  createDirectoryIfMissing True dir
  appendFile file (date ++ "\n")

readFromFile :: String -> IO [String]
readFromFile file = do
  contents <- readFile file
  let list = lines contents
  return list

fileName :: String -> String
fileName file = dir ++ "/" ++ file ++ ".txt"

removeLast :: [String] -> [String]
removeLast [] = []
removeLast xs = init xs
