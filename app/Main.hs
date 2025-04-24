
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (assert)
import Data.Char (toUpper)
import ArgsParser (parseArgs, ConvertInfo(..))
import Document (Document(..), Header(..), Content(..))
import ParsingLibrary (run)
import xmlParser (parseXml)

-- | Test XML parsing with a simple example
testXmlParser :: IO ()
testXmlParser = do
    let simpleXml = "<document>\
                    \  <header title=\"Simple Test\" author=\"Tester\" date=\"2024-04-24\"></header>\
                    \  <body>\
                    \    <paragraph>This is a simple test.</paragraph>\
                    \    <section>This is a section.</section>\
                    \  </body>\
                    \</document>"

    putStrLn "Testing XML Parser with simple XML..."
    case run parseXml simpleXml of
        Nothing -> do
            putStrLn "❌ Failed to parse XML!"
            exitWith (ExitFailure 84)
        Just (doc, remaining) -> do
            putStrLn "✅ Successfully parsed XML!"
            putStrLn $ "Document title: " ++ title (header doc)
            putStrLn $ "Document author: " ++ show (author (header doc))
            putStrLn $ "Document date: " ++ show (date (header doc))
            putStrLn $ "Content count: " ++ show (length (content doc))
            putStrLn $ "Remaining text: " ++ show remaining
            putStrLn "XML parsing test completed successfully."

-- | Test XML parser with a more complex example
testComplexXml :: IO ()
testComplexXml = do
    let complexXml = "<document>\
                     \  <header title=\"Complex Example\" author=\"Advanced User\"></header>\
                     \  <body>\
                     \    <section title=\"Introduction\">\
                     \      <paragraph>This is a paragraph in a section.</paragraph>\
                     \      <paragraph>This is another paragraph.</paragraph>\
                     \    </section>\
                     \    <paragraph>This is outside any section.</paragraph>\
                     \  </body>\
                     \</document>"

    putStrLn "\nTesting XML Parser with complex XML..."
    case run parseXml complexXml of
        Nothing -> do
            putStrLn "❌ Failed to parse complex XML!"
            exitWith (ExitFailure 84)
        Just (doc, _) -> do
            putStrLn "✅ Successfully parsed complex XML!"
            putStrLn $ "Content count: " ++ show (length (content doc))
            putStrLn "Complex XML test completed successfully."

-- | Main function to run the tests
main :: IO ()
main = do
    args <- getArgs

    if null args || head args == "test"
        then do
            putStrLn "Running XML Parser tests..."
            testXmlParser
            testComplexXml
            putStrLn "\nAll tests completed successfully!"
        else do
            -- Original main code for when not testing
            convertInfo <- parseArgs args
            putStrLn "Running in normal mode (not testing)"
            -- You can add your normal application logic here
            return ()
