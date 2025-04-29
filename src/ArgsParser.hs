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
import Control.Monad (when, unless, (>=>))
import FileStatus (validateFile, detectFormat) 

-- | Data structure to store parsed command-line arguments
data ConvertInfo = ConvertInfo
    { inputFile :: FilePath          -- Path to the input file
    , outputFormat :: String         -- Output format (xml, json, markdown)
    , outputFile :: Maybe FilePath   -- Optional path to the output file
    , inputFormat :: Maybe String    -- Optional input format (xml, json, markdown)
    } deriving (Show)

-- | Display an error message and exit with code 84
usageError :: String -> IO a
usageError msg = hPutStrLn stderr ("ERROR: " ++ msg) *>
    hPutStrLn stderr usageMessage *>
    exitWith (ExitFailure 84)
  where
    usageMessage = unlines
        [ "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
        , ""
        , "  ifile\t\tpath to the file to convert"
        , "  oformat\toutput format (xml, json, markdown)"
        , "  ofile\t\tpath to the output file"
        , "  iformat\tinput format (xml, json, markdown)"
        ]

-- | List of allowed formats
allowedFormats :: [String]
allowedFormats = ["xml", "json", "markdown"]

-- | turns args to lowercase and checks if it corresponds to one of ours
validateFormat :: String -> IO ()
validateFormat fmt =
    if map toLower fmt `elem` allowedFormats
        then return ()
        else usageError $ "Invalid format: " ++ fmt

-- | Basically get's all the info you need in the right place, also takes care of handling optionnal flags and whatknot
parseArgs :: [String] -> IO ConvertInfo
parseArgs args =
    when (odd $ length args) (usageError "Invalid number of arguments") *>
    let pairs = toPairs args in
    checkValidFlags pairs *>
    checkRequiredFlags pairs *>
    checkDuplicates pairs *>
    getRequired "-i" pairs >>=
    \inputFile ->
    getRequired "-f" pairs >>=
    \outputFormat ->
    getOptional "-o" pairs >>=
    \outputFile ->
    getOptional "-e" pairs >>=
    \inputFormat ->
    maybe (detectFormat inputFile >>= \detectedFormat ->
               validateFormat outputFormat *>
               validateFormat detectedFormat *>
               validateFile detectedFormat inputFile *>
               return (ConvertInfo inputFile outputFormat outputFile (Just detectedFormat))
          )
          (\providedFormat ->
               validateFormat outputFormat *>
               validateFormat providedFormat *>
               validateFile providedFormat inputFile *>
               return (ConvertInfo inputFile outputFormat outputFile (Just providedFormat))
          )
          inputFormat
  where
    -- | converts to pairs so that flag and data are together
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

    -- | Checks 4 mandatory flags
    checkRequiredFlags :: [(String, String)] -> IO ()
    checkRequiredFlags pairs =
        unless ("-i" `elem` flags) (usageError "Missing required flag: -i") *>
        unless ("-f" `elem` flags) (usageError "Missing required flag: -f")
      where flags = map fst pairs

    -- | Check for dups
    checkDuplicates :: [(String, String)] -> IO ()
    checkDuplicates pairs = case duplicates of
        [] -> return ()
        ds -> usageError $ "Duplicate flags: " ++ unwords ds
      where
        flags = map fst pairs
        duplicates = flags \\ nub flags

    -- | Retrieve the value for a required flag
    getRequired :: String -> [(String, String)] -> IO String
    getRequired flag pairs = maybe (usageError $ "Missing value for " ++ flag) return (lookup flag pairs)

    -- | Retrieve the value for an optional flag
    getOptional :: String -> [(String, String)] -> IO (Maybe String)
    getOptional flag pairs = return (lookup flag pairs)
