{-
-- EPITECH PROJECT, 2024
-- Main.hs
-- File description:
-- pandoc
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (writeFile)
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

-- | Main entry point of the program
main :: IO ()
main = do
    args <- getArgs
    if null args
        then exitWithError "No arguments provided"
        else processArgs args

-- | Process command line arguments
processArgs :: [String] -> IO ()
processArgs args = 
    parseArgs args >>= 
    \convertInfo ->
        let inputFile' = inputFile convertInfo
            inputFormat' = inputFormat convertInfo
            outputFormat' = outputFormat convertInfo
            outputFile' = outputFile convertInfo
        in
        -- Validate and detect format
        (case inputFormat' of
            Nothing -> 
                detectFormat inputFile' >>= 
                \detectedFormat -> validateFile detectedFormat inputFile' *>
                parseInputFile convertInfo detectedFormat
            Just fmt -> 
                validateFile fmt inputFile' *>
                parseInputFile convertInfo fmt) >>=
        -- Convert to output format
        \document ->
            let outputContent = convertDocument document outputFormat' in
            -- Write output
            case outputFile' of
                Just path -> writeFile path outputContent
                Nothing -> putStrLn outputContent

-- | Parse input file based on format
parseInputFile :: ConvertInfo -> String -> IO Document
parseInputFile convertInfo fmt =
    readFile (inputFile convertInfo) >>=
    \content ->
        case fmt of
            "xml" -> 
                case run parseXml content of
                    Just (doc, _) -> return doc
                    Nothing -> exitWithError "Failed to parse XML input"
            "json" ->
                case run parseJson content of
                    Just (doc, _) -> return doc
                    Nothing -> exitWithError "Failed to parse JSON input"
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
    putStrLn ("ERROR: " ++ msg) *>
    exitWith (ExitFailure 84)
