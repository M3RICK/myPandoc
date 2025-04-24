module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, SomeException)
import Data.Char (toUpper)
import ArgsParser (parseArgs, ConvertInfo(..))
import Document (Document(..), Header(..), Content(..), Inline(..))
import ParsingLibrary (run, char, stringP, Parser(..))  -- Added Parser here
import XmlParser
    ( parseXml
    , XmlParseResult
    , parseDocBegin
    , parseHeaderSection
    , parseBodyContent
    , parseDocEnd
    )

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

-- | Very simple XML for testing
minimalXml :: String
minimalXml = "<document><header title=\"Test\"></header><body></body></document>"

-- | Simple XML with some content
simpleXml :: String
simpleXml = "<document>\
            \  <header title=\"Simple Test\" author=\"Tester\" date=\"2024-04-24\"></header>\
            \  <body>\
            \    <paragraph>This is a simple test.</paragraph>\
            \    <section>This is a section.</section>\
            \  </body>\
            \</document>"

-- | Test with step-by-step debugging
testStepByStep :: IO ()
testStepByStep = do
    putStrLn "\n==== STEP-BY-STEP DEBUGGING ===="
    putStrLn "Testing with minimal XML:"
    putStrLn minimalXml

    -- Test very basic parsing
    _ <- debugParser "parsing '<'" (char '<') minimalXml
    _ <- debugParser "parsing '<document>'" (stringP "<document>") minimalXml

    -- Test the document begin parser
    mDocBegin <- debugParser "document begin tag" parseDocBegin minimalXml
    case mDocBegin of
        Nothing -> putStrLn "Cannot continue testing - document begin tag failed"
        Just (_, afterDocBegin) -> do
            -- Test header parsing
            mHeader <- debugParser "header section" parseHeaderSection afterDocBegin
            case mHeader of
                Nothing -> putStrLn "Cannot continue testing - header parsing failed"
                Just (hdr, afterHeader) -> do
                    -- Test body parsing
                    mBody <- debugParser "body content" parseBodyContent afterHeader
                    case mBody of
                        Nothing -> putStrLn "Cannot continue testing - body parsing failed"
                        Just (bodyContent, afterBody) -> do
                            -- Test document end tag
                            _ <- debugParser "document end tag" parseDocEnd afterBody

                            -- Test complete document parsing
                            _ <- debugParser "complete document" parseXml minimalXml
                            return ()

-- | Main test function
testXmlParser :: IO ()
testXmlParser = do
    putStrLn "\n==== XML PARSER TEST ===="
    putStrLn "Testing with simple XML:"
    putStrLn simpleXml

    -- First try with simple XML
    case run parseXml simpleXml of
        Nothing -> do
            putStrLn "❌ Failed to parse simple XML"
            -- If simple XML fails, try with minimal XML
            putStrLn "\nTrying with minimal XML instead:"
            putStrLn minimalXml
            case run parseXml minimalXml of
                Nothing -> do
                    putStrLn "❌ Failed to parse even minimal XML"
                    testStepByStep  -- Try step-by-step debugging
                Just (doc, _) -> do
                    putStrLn "✅ Minimal XML parsed successfully"
                    putStrLn $ "Document title: " ++ title (header doc)
        Just (doc, remaining) -> do
            putStrLn "✅ Successfully parsed XML!"
            putStrLn $ "Document title: " ++ title (header doc)
            putStrLn $ "Document author: " ++ show (author (header doc))
            putStrLn $ "Document date: " ++ show (date (header doc))
            putStrLn $ "Content count: " ++ show (length (content doc))
            putStrLn $ "Remaining text: " ++ show remaining

-- | Main function
main :: IO ()
main = do
    args <- getArgs

    if null args || head args == "test"
        then do
            putStrLn "Running XML Parser tests with debugging..."
            testXmlParser
            putStrLn "\nTests completed."
        else do
            -- Original main code for when not testing
            convertInfo <- parseArgs args
            putStrLn "Running in normal mode (not testing)"
            -- You can add your normal application logic here
            return ()
