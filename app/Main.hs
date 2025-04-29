{-
-- EPITECH PROJECT, 2024
-- Main.hs
-- File description:
-- pandoc
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (writeFile, hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))
import ArgsParser (parseArgs, ConvertInfo(..))
import Document (Document(..), Header(..), Content(..), Inline(..))
import XmlParser (parseXml)
import JsonParser (parseJson)
import FileStatus (validateFile, detectFormat)
import XmlOut (documentToXml)
import JsonOut (documentToJson)
import MarkdownOut (documentToMarkdown)
import ParsingLibrary (run)
import Data.Maybe (isNothing)
import Control.Monad (when)

-- | Main entry point of the program
main :: IO ()
main = do
    args <- getArgs
    if null args
        then exitWithError "No arguments provided"
        else processArgs args

-- | Process command line arguments
processArgs :: [String] -> IO ()
processArgs args = do
    convertInfo <- parseArgs args
    let inputFile' = inputFile convertInfo
        inputFormat' = inputFormat convertInfo
        outputFormat' = outputFormat convertInfo
        outputFile' = outputFile convertInfo
    
    -- Determine and validate format
    (actualFormat, document) <- case inputFormat' of
        Nothing -> do
            detectedFormat <- detectFormat inputFile'
            putStrLn $ "INFO: Detected input format: " ++ detectedFormat
            validateFile detectedFormat inputFile'
            doc <- parseInputFile convertInfo detectedFormat
            return (detectedFormat, doc)
        Just fmt -> do
            putStrLn $ "INFO: Using specified input format: " ++ fmt
            validateFile fmt inputFile'
            doc <- parseInputFile convertInfo fmt
            return (fmt, doc)
    
    -- Debug print document structure
    putStrLn $ "DEBUG: Document header: " ++ show (header document)
    putStrLn $ "DEBUG: Document content count: " ++ show (length (content document))
    
    -- Convert to output format
    putStrLn $ "INFO: Converting from " ++ actualFormat ++ " to " ++ outputFormat'
    let outputContent = convertDocument document outputFormat'
    
    -- Write output
    case outputFile' of
        Just path -> do
            putStrLn $ "INFO: Writing to output file: " ++ path
            writeFile path outputContent
            putStrLn $ "INFO: Conversion completed successfully"
        Nothing -> do
            putStrLn "INFO: Output to stdout:"
            putStrLn outputContent

-- | Parse input file based on format
parseInputFile :: ConvertInfo -> String -> IO Document
parseInputFile convertInfo fmt = do
    content <- readFile (inputFile convertInfo)
    putStrLn $ "INFO: Parsing input file: " ++ inputFile convertInfo
    putStrLn $ "DEBUG: First 50 chars of file: " ++ take 50 content
    
    case fmt of
        "xml" -> do
            putStrLn "DEBUG: Parsing XML content"
            let result = run parseXml content
            if isNothing result
                then do
                    hPutStrLn stderr $ "DEBUG: XML parsing failed"
                    hPutStrLn stderr $ "DEBUG: Content sample:\n" ++ take 100 content
                    exitWithError "Failed to parse XML input"
                else do
                    putStrLn "INFO: XML parsing successful"
                    let Just (doc, rest) = result
                    when (not (null rest)) $
                        putStrLn $ "WARN: Parser did not consume all input. Remaining: " ++ take 50 rest
                    return doc
        "json" -> do
            putStrLn "DEBUG: Parsing JSON content"
            let result = run parseJson content
            if isNothing result
                then do
                    hPutStrLn stderr $ "DEBUG: JSON parsing failed"
                    hPutStrLn stderr $ "DEBUG: Content sample:\n" ++ take 100 content
                    exitWithError "Failed to parse JSON input"
                else do
                    putStrLn "INFO: JSON parsing successful"
                    let Just (doc, rest) = result
                    when (not (null rest)) $
                        putStrLn $ "WARN: Parser did not consume all input. Remaining: " ++ take 50 rest
                    return doc
        _ -> exitWithError ("Unsupported input format: " ++ fmt)

-- | Convert document to specified output format
convertDocument :: Document -> String -> String
convertDocument document format =
    case format of
        "xml" -> documentToXml document
        "json" -> documentToJson document
        "markdown" -> documentToMarkdown document
        _ -> error ("Unsupported output format: " ++ format)

-- | Exit with error message and code 84
exitWithError :: String -> IO a
exitWithError msg = do
    hPutStrLn stderr ("ERROR: " ++ msg)
    exitWith (ExitFailure 84)