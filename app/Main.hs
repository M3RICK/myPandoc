{-
-- EPITECH PROJECT, 2024
-- Main.hs
-- File description:
-- pandoc
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, writeFile)
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

-- | Process command line arguments
processArgs :: [String] -> IO ()
processArgs args = 
    parseArgs args >>= \convertInfo ->
        let inputFile' = inputFile convertInfo
            inputFormat' = inputFormat convertInfo
            outputFormat' = outputFormat convertInfo
            outputFile' = outputFile convertInfo
        in
        -- Determine and validate format
        (case inputFormat' of
            Nothing -> 
                detectFormat inputFile' >>= \detectedFormat ->
                validateFile detectedFormat inputFile' >>
                parseInputFile convertInfo detectedFormat >>= \doc ->
                return (detectedFormat, doc)
            Just fmt -> 
                validateFile fmt inputFile' >>
                parseInputFile convertInfo fmt >>= \doc ->
                return (fmt, doc)) >>= \(_, document) ->
        
        -- Convert to output format
        let outputContent = convertDocument document outputFormat' in
        
        -- Write output
        case outputFile' of
            Just path -> 
                writeFile path outputContent
            Nothing -> 
                putStrLn outputContent

-- | Parse input file based on format
parseInputFile :: ConvertInfo -> String -> IO Document
parseInputFile convertInfo fmt = 
    readFile (inputFile convertInfo) >>= \content' ->
    
    case fmt of
        "xml" -> 
            let result = run parseXml content' in
            if isNothing result
                then 
                    exitWithError "Failed to parse XML input"
                else
                    let Just (doc, _) = result in
                    return doc
        "json" -> 
            let result = run parseJson content' in
            if isNothing result
                then 
                    exitWithError "Failed to parse JSON input"
                else
                    let Just (doc, _) = result in
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
exitWithError msg = 
    hPutStrLn stderr ("ERROR: " ++ msg) >>
    exitWith (ExitFailure 84)