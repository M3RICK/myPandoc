module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (assert)
import Data.Char (toUpper)
import ArgsParser (parseArgs, ConvertInfo(..))
import Document (Document)
import ParsingLibrary (run)

main :: IO ()
main = do
    args <- getArgs
    convertInfo <- parseArgs args
    return ()
-- main = do
--   args <- getArgs
--   putStrLn "Running ParserLib tests..."

--   -- char
--   assert (run (char 'a') "abc" == Just ('a', "bc")) $ putStrLn "char test passed"

--   -- oneOf
--   assert (run (oneOf "aeiou") "apple" == Just ('a', "pple")) $ putStrLn "oneOf test passed"

--   -- noneOf
--   assert (run (noneOf "xyz") "hello" == Just ('h', "ello")) $ putStrLn "noneOf test passed"

--   -- satisfy
--   assert (run (satisfy (== 'z')) "zoo" == Just ('z', "oo")) $ putStrLn "satisfy test passed"

--   -- orElse
--   assert (run (char 'x' `orElse` char 'y') "yup" == Just ('y', "up")) $ putStrLn "orElse test passed"

--   -- pairP
--   assert (run (pairP (char 'a') (char 'b')) "abc" == Just (('a','b'), "c")) $ putStrLn "pairP test passed"

--   -- mapP
--   assert (run (mapP toUpper (char 'a')) "abc" == Just ('A', "bc")) $ putStrLn "mapP test passed"

--   -- map2P
--   assert (run (map2P (:) (char 'a') (manyP (char 'b'))) "abbb" == Just ("abbb", "")) $ putStrLn "map2P test passed"

--   -- manyP
--   assert (run (manyP (char 'x')) "xxxy" == Just ("xxx", "y")) $ putStrLn "manyP test passed"

--   -- someP
--   assert (run (someP digit) "123abc" == Just ("123", "abc")) $ putStrLn "someP test passed"

--   -- optionP
--   assert (run (optionP 'z' (char 'x')) "abc" == Just ('z', "abc")) $ putStrLn "optionP test passed"

--   -- stringP
--   assert (run (stringP "hello") "hello world" == Just ("hello", " world")) $ putStrLn "stringP test passed"

--   -- digit
--   assert (run digit "9abc" == Just ('9', "abc")) $ putStrLn "digit test passed"

--   -- letter
--   assert (run letter "h3llo" == Just ('h', "3llo")) $ putStrLn "letter test passed"

--   -- alphaNum
--   assert (run alphaNum "a9!" == Just ('a', "9!")) $ putStrLn "alphaNum test passed"

--   -- space
--   assert (run space "  abc" == Just (' ', " abc")) $ putStrLn "space test passed"

--   -- skipSpaces
--   assert (run skipSpaces "   abc" == Just ("   ", "abc")) $ putStrLn "skipSpaces test passed"

--   -- token
--   assert (run (token (char 'a')) "a   bc" == Just ('a', "bc")) $ putStrLn "token test passed"

--   -- whitespaces
--   assert (run whitespaces " \t\nx" == Just (" \t\n", "x")) $ putStrLn "whitespaces test passed"

--   -- skipWhitespace
--   assert (run skipWhitespace " \n\rabc" == Just (" \n\r", "abc")) $ putStrLn "skipWhitespace test passed"

--   -- word
--   assert (run word "hello123 world" == Just ("hello123", " world")) $ putStrLn "word test passed"

--   -- exactWord
--   assert (run (exactWord "hello") "hello world" == Just ("hello", " world")) $ putStrLn "exactWord test passed"
--   assert (run (exactWord "hello") "hellothere" == Nothing) $ putStrLn "exactWord failure test passed"

--   -- natural
--   assert (run natural "1234abc" == Just (1234, "abc")) $ putStrLn "natural test passed"

--   -- intingeger (yes thierry)
--   assert (run intingeger "-42x" == Just (-42, "x")) $ putStrLn "intingeger (-) test passed"
--   assert (run intingeger "13x" == Just (13, "x")) $ putStrLn "intingeger (+) test passed"

--   -- tupleP
--   assert (run (tupleP natural) "(1,2)x" == Just ((1,2), "x")) $ putStrLn "tupleP test passed"


--   putStrLn "✅ All tests passed!"
