module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, SomeException)
import Data.Char (toUpper)
import ArgsParser (parseArgs, ConvertInfo(..))
import Document (Document(..), Header(..), Content(..), Inline(..))
import ParsingLibrary (run, char, stringP, Parser(..))
import XmlParser (parseXml, parseDocBegin, parseHeaderSection, parseBodyContent, parseDocEnd)
import JsonParser (parseJson)

-- | Safely run a parser and show debug info
debugParser :: Show a => String -> Parser a -> String -> IO (Maybe (a, String))
debugParser label parser input = do
    putStrLn $ "\n=== Testing " ++ label ++ " ==="
    putStrLn $ "Input: " ++ take 40 input ++ (if length input > 40 then "..." else "")

    -- Safely run the parser
    result <- catch
        (let r = run parser input in r `seq` return r)
        (\e -> do
            putStrLn $ "❌ Exception: " ++ show (e :: SomeException)
            return Nothing
        )

    case result of
        Nothing -> do
            putStrLn "❌ Parser failed"
            return Nothing
        Just (val, rest) -> do
            putStrLn "✅ Parser succeeded!"
            putStrLn $ "Value: " ++ show val
            putStrLn $ "Remaining: " ++ take 40 rest ++ (if length rest > 40 then "..." else "")
            return $ Just (val, rest)

    return result

-------------------------------------
-- Example test inputs
-------------------------------------

-- | Very simple XML for testing
minimalXml :: String
minimalXml = "<document><header title=\"Test\"></header><body></body></document>"

-- | Simple XML with some content
simpleXml :: String
simpleXml = "<document>\
            \  <header title=\"Simple Test\" author=\"Tester\" date=\"2024-04-24\"></header>\
            \  <body>\
            \    <paragraph>This is a simple test.</paragraph>\
            \    <section title=\"Section Title\"></section>\
            \  </body>\
            \</document>"

-- | Minimal JSON example
minimalJson :: String
minimalJson = "{ \"header\": { \"title\": \"Test\" }, \"body\": [] }"

-- | Simple JSON example with body text
simpleJson :: String
simpleJson = "{\
            \  \"header\": {\
            \    \"title\": \"Simple Test\",\
            \    \"author\": \"Tester\",\
            \    \"date\": \"2024-04-24\"\
            \  },\
            \  \"body\": [\
            \    \"This is a simple test.\",\
            \    { \"codeblock\": \"print('Hello')\" },\
            \    { \"list\": [\"Item 1\", \"Item 2\"] }\
            \  ]\
            \}"

-------------------------------------
-- Testing functions
-------------------------------------

-- | Test the XML parser
testXmlParser :: IO ()
testXmlParser = do
    putStrLn "\n==== XML PARSER TEST ===="
    putStrLn "Testing with simple XML:"
    putStrLn simpleXml

    case run parseXml simpleXml of
        Nothing -> do
            putStrLn "❌ Failed to parse simple XML"
            putStrLn "\nTrying with minimal XML instead:"
            putStrLn minimalXml
            case run parseXml minimalXml of
                Nothing -> do
                    putStrLn "❌ Failed to parse even minimal XML"
                    testStepByStepXml
                Just (doc, _) -> do
                    putStrLn "✅ Minimal XML parsed successfully"
                    printBasicDocumentInfo doc
        Just (doc, remaining) -> do
            putStrLn "✅ Successfully parsed XML!"
            printBasicDocumentInfo doc
            putStrLn $ "Remaining text: " ++ show remaining

-- | Test the JSON parser
testJsonParser :: IO ()
testJsonParser = do
    putStrLn "\n==== JSON PARSER TEST ===="
    putStrLn "Testing with simple JSON:"
    putStrLn simpleJson

    case run parseJson simpleJson of
        Nothing -> do
            putStrLn "❌ Failed to parse simple JSON"
            putStrLn "\nTrying with minimal JSON instead:"
            putStrLn minimalJson
            case run parseJson minimalJson of
                Nothing -> do
                    putStrLn "❌ Failed to parse even minimal JSON"
                Just (doc, _) -> do
                    putStrLn "✅ Minimal JSON parsed successfully"
                    printBasicDocumentInfo doc
        Just (doc, remaining) -> do
            putStrLn "✅ Successfully parsed JSON!"
            printBasicDocumentInfo doc
            putStrLn $ "Remaining text: " ++ show remaining

-- | Print basic document info
printBasicDocumentInfo :: Document -> IO ()
printBasicDocumentInfo doc = do
    putStrLn $ "Document title: " ++ title (header doc)
    putStrLn $ "Document author: " ++ show (author (header doc))
    putStrLn $ "Document date: " ++ show (date (header doc))
    putStrLn $ "Content count: " ++ show (length (content doc))

-- | Test step-by-step XML parsing
testStepByStepXml :: IO ()
testStepByStepXml = do
    putStrLn "\n==== STEP-BY-STEP DEBUGGING ===="
    putStrLn "Testing with minimal XML:"
    putStrLn minimalXml

    _ <- debugParser "parsing '<'" (char '<') minimalXml
    _ <- debugParser "parsing '<document>'" (stringP "<document>") minimalXml

    mDocBegin <- debugParser "document begin tag" parseDocBegin minimalXml
    case mDocBegin of
        Nothing -> putStrLn "Cannot continue testing - document begin tag failed"
        Just (_, afterDocBegin) -> do
            mHeader <- debugParser "header section" parseHeaderSection afterDocBegin
            case mHeader of
                Nothing -> putStrLn "Cannot continue testing - header parsing failed"
                Just (hdr, afterHeader) -> do
                    mBody <- debugParser "body content" parseBodyContent afterHeader
                    case mBody of
                        Nothing -> putStrLn "Cannot continue testing - body parsing failed"
                        Just (bodyContent, afterBody) -> do
                            _ <- debugParser "document end tag" parseDocEnd afterBody
                            _ <- debugParser "complete document" parseXml minimalXml
                            return ()

-------------------------------------
-- Main program
-------------------------------------

-- | Main function
main :: IO ()
main = do
    args <- getArgs

    case args of
        [] -> do
            putStrLn "Running default (XML) tests..."
            testXmlParser
        ["test"] -> do
            putStrLn "Running default (XML) tests..."
            testXmlParser
        ["test-xml"] -> do
            putStrLn "Running XML parser tests..."
            testXmlParser
        ["test-json"] -> do
            putStrLn "Running JSON parser tests..."
            testJsonParser
        _ -> do
            putStrLn "Running in normal conversion mode (no tests)..."
            convertInfo <- parseArgs args
            -- Your normal document conversion code goes here (not written yet)
            return ()
