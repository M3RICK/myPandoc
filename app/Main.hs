{-
-- EPITECH PROJECT, 2024
-- Main.hs
-- File description:
-- pandoc
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))
import ArgsParser (parseArgs, ConvertInfo(..))
import Document (Document(..))
import XmlParser (parseXml)
import JsonParser (parseJson)
import FileStatus (validateFile, detectFormat)
import XmlOut (documentToXml)
import JsonOut (documentToJson)
import MarkdownOut (documentToMarkdown)
import ParsingLibrary (run)
import Data.Maybe (isNothing)

-- | Main entry point of the program
main :: IO ()
main = do
    args <- getArgs
    if null args
        then exitWithError "No arguments provided"
        else processArgs args

-- | Handle output writing
writeOutput :: Maybe FilePath -> String -> IO ()
writeOutput outputFile' outputContent =
    case outputFile' of
        Just path -> writeFile path outputContent
        Nothing -> putStrLn outputContent

-- | Process document conversion
convertAndOutput :: Document -> String -> Maybe FilePath -> IO ()
convertAndOutput document outputFormat' outputFile' =
    let outputContent = convertDocument document outputFormat' in
    writeOutput outputFile' outputContent

-- | Handle detected format
handleDetectedFormat :: ConvertInfo -> IO (String, Document)
handleDetectedFormat convertInfo =
    detectFormat (inputFile convertInfo) >>= \detectedFormat ->
    validateFile detectedFormat (inputFile convertInfo) >>
    parseInputFile convertInfo detectedFormat >>= \doc ->
    return (detectedFormat, doc)

-- | Handle specified format
handleSpecifiedFormat :: ConvertInfo -> String -> IO (String, Document)
handleSpecifiedFormat convertInfo fmt =
    validateFile fmt (inputFile convertInfo) >>
    parseInputFile convertInfo fmt >>= \doc ->
    return (fmt, doc)

-- | Prepare command-line info 
prepareInfo :: ConvertInfo -> (Maybe String, String, Maybe FilePath)
prepareInfo convertInfo =
    let inputFormat' = inputFormat convertInfo
        outputFormat' = outputFormat convertInfo
        outputFile' = outputFile convertInfo
    in (inputFormat', outputFormat', outputFile')

-- | Handle format detection and validation
handleFormat :: ConvertInfo -> IO (String, Document)
handleFormat convertInfo =
    let (inputFormat', _, _) = prepareInfo convertInfo in
    case inputFormat' of
        Nothing -> handleDetectedFormat convertInfo
        Just fmt -> handleSpecifiedFormat convertInfo fmt

-- | Process command line arguments
processArgs :: [String] -> IO ()
processArgs args =
    parseArgs args >>= \convertInfo ->
        let (_, outputFormat', outputFile') = prepareInfo convertInfo in
        handleFormat convertInfo >>= \(_, document) ->
        convertAndOutput document outputFormat' outputFile'

-- | Parse XML input
parseXmlInput :: String -> IO Document
parseXmlInput content' =
    let result = run parseXml content' in
    if isNothing result
        then exitWithError "Failed to parse XML input"
        else case result of
            Just (doc, _) -> return doc
            Nothing -> exitWithError "Failed to parse input"

-- | Parse JSON input
parseJsonInput :: String -> IO Document
parseJsonInput content' =
    let result = run parseJson content' in
    if isNothing result
        then exitWithError "Failed to parse JSON input"
        else case result of
            Just (doc, _) -> return doc
            Nothing -> exitWithError "Failed to parse input"

-- | Parse input file based on format
parseInputFile :: ConvertInfo -> String -> IO Document
parseInputFile convertInfo fmt =
    readFile (inputFile convertInfo) >>= \content' ->
        case fmt of
            "xml" -> parseXmlInput content'
            "json" -> parseJsonInput content'
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
exitWithError msg = 
    hPutStrLn stderr ("ERROR: " ++ msg) >>
    exitWith (ExitFailure 84)