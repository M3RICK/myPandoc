{-
-- EPITECH PROJECT, 2024
-- ArgsParser.hs
-- File description:
-- Command-line argument parsing for mypandoc
-}

module ArgsParser
    ( ConvertInfo(..)
    , parseArgs
    ) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Data.Char (toLower)
import Data.List ((\\), nub)
import Control.Monad (when, unless)
import FileStatus (validateFile, detectFormat)

-- | Data structure to store parsed command-line arguments
data ConvertInfo = ConvertInfo
    { inputFile :: FilePath          -- Path to the input file
    , outputFormat :: String         -- Output format (xml, json, markdown)
    , outputFile :: Maybe FilePath   -- Optional path to the output file
    , inputFormat :: Maybe String    -- Optional input format (xml, json, markdown)
    } deriving (Show)

-- | Usage message for the program
usageMessage :: String
usageMessage = unlines
    [ "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
    , ""
    , "  ifile\t\tpath to the file to convert"
    , "  oformat\toutput format (xml, json, markdown)"
    , "  ofile\t\tpath to the output file"
    , "  iformat\tinput format (xml, json, markdown)"
    ]

-- | Display an error message and exit with code 84
usageError :: String -> IO a
usageError msg = hPutStrLn stderr ("ERROR: " ++ msg) >>
    hPutStrLn stderr usageMessage >>
    exitWith (ExitFailure 84)

-- | List of allowed formats
allowedFormats :: [String]
allowedFormats = ["xml", "json", "markdown"]

-- | turns args to lowercase and checks if it corresponds to one of ours
validateFormat :: String -> IO ()
validateFormat fmt =
    if map toLower fmt `elem` allowedFormats
        then return ()
        else usageError $ "Invalid format: " ++ fmt

-- | Create argument pairs from list
toPairs :: [String] -> [(String, String)]
toPairs [] = []
toPairs (f:v:rest) = (f, v) : toPairs rest
toPairs _ = error "Internal error: invalid argument structure"

-- | Check that all flags are valid
checkValidFlags :: [(String, String)] -> IO ()
checkValidFlags pairs = mapM_ checkPair pairs
  where
    valid = ["-i", "-f", "-o", "-e"]
    checkPair (f, _) | f `elem` valid = return ()
                     | otherwise = usageError $ "Invalid flag: " ++ f

-- | Checks required flags
checkRequiredFlags :: [(String, String)] -> IO ()
checkRequiredFlags pairs =
    unless ("-i" `elem` flags) (usageError "Missing required flag: -i") >>
    unless ("-f" `elem` flags) (usageError "Missing required flag: -f")
  where flags = map fst pairs

-- | Check for duplicates
checkDuplicates :: [(String, String)] -> IO ()
checkDuplicates pairs = case duplicates of
    [] -> return ()
    ds -> usageError $ "Duplicate flags: " ++ unwords ds
  where
    flags = map fst pairs
    duplicates = flags \\ nub flags

-- | Retrieve the value for a required flag
getRequired :: String -> [(String, String)] -> IO String
getRequired flag pairs =
    maybe (usageError $ missingValueMsg flag) return (lookup flag pairs)
    where
      missingValueMsg f = "Missing value for " ++ f

-- | Retrieve the value for an optional flag
getOptional :: String -> [(String, String)] -> IO (Maybe String)
getOptional flag pairs = return (lookup flag pairs)

-- | Validate file with format
validateFileWithFormat :: String -> String -> FilePath -> IO ()
validateFileWithFormat formatStr fmt filePath =
    validateFormat formatStr >>
    validateFormat fmt >>
    validateFile fmt filePath

-- | Create ConvertInfo with detected format
createWithDetectedFormat :: FilePath
                        -> String
                        -> Maybe FilePath
                        -> String
                        -> IO ConvertInfo
createWithDetectedFormat filePath formatStr outFilePath fmt =
    return (ConvertInfo filePath formatStr outFilePath (Just fmt))

-- | Process detected format
processDetectedFormat :: FilePath -> String -> Maybe FilePath -> IO ConvertInfo
processDetectedFormat filePath formatStr outFilePath =
    detectFormat filePath >>= \fmt ->
    validateFileWithFormat formatStr fmt filePath >>
    createWithDetectedFormat filePath formatStr outFilePath fmt

-- | Process provided format
processProvidedFormat :: FilePath
                     -> String
                     -> Maybe FilePath
                     -> String
                     -> IO ConvertInfo
processProvidedFormat filePath formatStr outFilePath providedFormat =
    validateFileWithFormat formatStr providedFormat filePath >>
    createWithDetectedFormat filePath formatStr outFilePath providedFormat

-- | Parse args to check if they're valid
checkArgs :: [String] -> IO [(String, String)]
checkArgs args =
    when (odd $ length args) (usageError "Invalid number of arguments") >>
    let pairs = toPairs args in
    checkValidFlags pairs >>
    checkRequiredFlags pairs >>
    checkDuplicates pairs >>
    return pairs

-- | Basically get's all the info you need in the right place
parseArgs :: [String] -> IO ConvertInfo
parseArgs args =
    checkArgs args >>= \pairs ->
    getRequired "-i" pairs >>= \filePath ->
    getRequired "-f" pairs >>= \formatStr ->
    getOptional "-o" pairs >>= \outFilePath ->
    getOptional "-e" pairs >>= \formatSpec ->
    maybe
        (processDetectedFormat filePath formatStr outFilePath)
        (processProvidedFormat filePath formatStr outFilePath)
        formatSpec
