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
import Control.Monad (unless, when)
import Data.Char (toLower, isSpace)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)

-- | Error displayer, you know the drill...
usageError :: String -> IO a
usageError msg =
  putStrLn ("ERROR: " ++ msg) >>
  exitWith (ExitFailure 84)

-- | Trim whitespace from start and end
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

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
validateXml content =
  let trimmed = trim content in
  -- More flexible XML detection
  ("<" `isPrefixOf` trimmed) &&
  (">" `isInfixOf` trimmed) &&
  ("</" `isInfixOf` trimmed) &&
  ("<document>" `isInfixOf` trimmed)

-- | Validate file content for JSON format
validateJson :: String -> Bool
validateJson content =
  let trimmed = trim content in
  -- More flexible JSON detection
  ("{" `isPrefixOf` trimmed) &&
  ("}" `isInfixOf` trimmed) &&
  ("\"header\"" `isInfixOf` trimmed)

-- | Validate file content for Markdown format
validateMarkdown :: String -> Bool
validateMarkdown content =
  -- Accept any non-empty content as potential Markdown
  -- Check for common Markdown elements
  let trimmed = trim content
  in not (all isSpace trimmed) &&
     ("---" `isPrefixOf` trimmed ||
      "#" `isPrefixOf` trimmed ||
      "*" `isInfixOf` trimmed ||
      "-" `isInfixOf` trimmed)

-- | checks more flexibly for correct format structure
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
  when isEmpty (handleEmptyFile filePath) >> -- Use 'when' instead of 'unless (not isEmpty)'
  isValidContent fmt filePath >>= \isValid ->
  unless isValid (handleInvalidContent fmt)

-- | more flexible way to detect format
detectFormat :: FilePath -> IO String
detectFormat filePath = do
  content <- readFile filePath
  let trimmed = trim content
  
  -- Improved format detection logic
  case () of
    _ | validateXml trimmed  -> return "xml"
      | validateJson trimmed -> return "json"
      | otherwise           -> return "markdown"  -- Default to markdown