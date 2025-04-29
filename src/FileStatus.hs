{-
-- EPITECH PROJECT, 2024
-- FileStatus.hs
-- File description:
-- Checks files exist, well formatted etc etc...
-}
module FileStatus
( validateFile
, detectFormat
) where

import System.IO (openFile, IOMode(ReadMode), hClose, hFileSize)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Control.Exception (try)
import Control.Monad (unless)
import Data.Char (toLower)

-- | Main func, checks if file exists, accessible, empty...
validateFile :: String -> FilePath -> IO ()
validateFile fmt filePath =
  doesFileExist' filePath >>= \exists ->
    if not exists
    then usageError $ "File does not exist: " ++ filePath
    else isFileEmpty filePath >>= \isEmpty ->
      if isEmpty
      then usageError $ "Input file is empty: " ++ filePath
      else isValidContent fmt filePath >>= \isValid ->
        unless isValid $ usageError $ "Input file contains invalid content for format: " ++ fmt

-- | bruteforce way to get format
detectFormat :: FilePath -> IO String
detectFormat filePath =
  readFile filePath >>= \content ->
    case content of
      ('<':_) -> return "xml"
      ('{':_) -> return "json"
      ('-':'-':'-':_) -> return "markdown"
      _ -> usageError $ "Unable to detect input format for file: " ++ filePath

-- | checks verry poorly for correct format structure
-- | checks verry poorly for correct format structure
isValidContent :: String -> FilePath -> IO Bool
isValidContent fmt filePath =
  readFile filePath >>= \content ->
    let result = case map toLower fmt of
          "xml" -> isPrefixOf "<document>" content
          "json" -> isPrefixOf "{" content
          "markdown" -> isPrefixOf "---" content
          _ -> False
    in do
      putStrLn $ "DEBUG: Checking if file content matches format " ++ fmt ++ ": " ++ show result
      putStrLn $ "DEBUG: First 20 chars: " ++ take 20 content
      return result
  where
    -- Helper function to check if a string is a prefix of another
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (p:ps) (s:ss) = p == s && isPrefixOf ps ss


-- | Self Explanatory
doesFileExist' :: FilePath -> IO Bool
doesFileExist' filePath =
  try (openFile filePath ReadMode) >>= \result ->
    case result of
      Left e
        | isDoesNotExistError e -> return False
        | otherwise -> return False
      Right handle -> hClose handle >> return True

-- | Self Explanatory
isFileEmpty :: FilePath -> IO Bool
isFileEmpty filePath =
  openFile filePath ReadMode >>= \handle ->
    hFileSize handle >>= \fileSize ->
      hClose handle >>
      return (fileSize == 0)

-- | Error displayer, you know the drill...
usageError :: String -> IO a
usageError msg =
  putStrLn ("ERROR: " ++ msg) >>
  exitWith (ExitFailure 84)