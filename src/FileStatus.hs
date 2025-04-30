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
import System.IO.Error (isDoesNotExistError)
import Control.Exception (try)
import Control.Monad (unless)
import Data.Char (toLower, isSpace)
import Data.List (dropWhileEnd)

-- | Error displayer, you know the drill...
usageError :: String -> IO a
usageError msg =
  putStrLn ("ERROR: " ++ msg) >>
  exitWith (ExitFailure 84)

-- | Trim whitespace from start and end
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Helper function to check if a string is a prefix of another
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (p:ps) (s:ss) = p == s && isPrefixOf ps ss

-- | Check if file exists
doesFileExist' :: FilePath -> IO Bool
doesFileExist' filePath =
  try (openFile filePath ReadMode) >>= \result ->
    case result of
      Left e
        | isDoesNotExistError e -> return False
        | otherwise -> return False
      Right handle -> hClose handle >> return True

-- | Check if file is empty
isFileEmpty :: FilePath -> IO Bool
isFileEmpty filePath =
  openFile filePath ReadMode >>= \handle ->
    hFileSize handle >>= \fileSize ->
      hClose handle >>
      return (fileSize == 0)

-- | Validate file content for XML format
validateXml :: String -> Bool
validateXml content = isPrefixOf "<document>" content

-- | Validate file content for JSON format
validateJson :: String -> Bool
validateJson content = isPrefixOf "{" content

-- | Validate file content for Markdown format
validateMarkdown :: String -> Bool
validateMarkdown content = isPrefixOf "---" content

-- | checks verry poorly for correct format structure
isValidContent :: String -> FilePath -> IO Bool
isValidContent fmt filePath = do
  content <- readFile filePath
  let trimmedContent = trim content
  return $ validateForFormat (map toLower fmt) trimmedContent
  where
    validateForFormat "xml" = validateXml
    validateForFormat "json" = validateJson
    validateForFormat "markdown" = validateMarkdown
    validateForFormat _ = const False

-- | Handle file not existing case
handleNonExistentFile :: FilePath -> IO ()
handleNonExistentFile filePath = 
  usageError $ "File does not exist: " ++ filePath

-- | Handle empty file case
handleEmptyFile :: FilePath -> IO ()
handleEmptyFile filePath = 
  usageError $ "Input file is empty: " ++ filePath

-- | Handle invalid content case
handleInvalidContent :: String -> IO ()
handleInvalidContent fmt = 
  usageError $ "Input file contains invalid content for format: " ++ fmt

-- | Main func, checks if file exists, accessible, empty...
validateFile :: String -> FilePath -> IO ()
validateFile fmt filePath =
  doesFileExist' filePath >>= \exists ->
  unless exists (handleNonExistentFile filePath) >>
  isFileEmpty filePath >>= \isEmpty ->
  unless (not isEmpty) (handleEmptyFile filePath) >>
  isValidContent fmt filePath >>= \isValid ->
  unless isValid (handleInvalidContent fmt)

-- | bruteforce way to get format
detectFormat :: FilePath -> IO String
detectFormat filePath = do
  content <- readFile filePath
  let trimmedContent = trim content
  identifyFormat trimmedContent
  where
    identifyFormat content
      | isPrefixOf "<" content = return "xml"
      | isPrefixOf "{" content = return "json"
      | isPrefixOf "---" content = return "markdown"
      | otherwise = usageError $ 
          "Unable to detect input format for file: " ++ filePath