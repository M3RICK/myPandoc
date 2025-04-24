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

data ConvertInfo = ConvertInfo
    { inputFile :: FilePath
    , outputFormat :: String
    , outputFile :: Maybe FilePath
    , inputFormat :: Maybe String
    } deriving (Show)

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

allowedFormats :: [String]
allowedFormats = ["xml", "json", "markdown"]

validateFormat :: String -> IO ()
validateFormat fmt = 
    if map toLower fmt `elem` allowedFormats
        then pure ()
        else usageError $ "Invalid format: " ++ fmt

parseArgs :: [String] -> IO ConvertInfo
parseArgs args =
    when (odd $ length args) (usageError "Invalid number of arguments") *>
    let pairs = toPairs args in
    checkValidFlags pairs *>
    checkRequiredFlags pairs *>
    checkDuplicates pairs *>
    (ConvertInfo
        <$> getRequired "-i" pairs
        <*> getRequired "-f" pairs
        <*> getOptional "-o" pairs
        <*> getOptional "-e" pairs)
    >>= \ci -> validateFormat (outputFormat ci) *>
               maybe (pure ()) validateFormat (inputFormat ci) *>
               pure ci
  where
    toPairs :: [String] -> [(String, String)]
    toPairs [] = []
    toPairs (f:v:rest) = (f, v) : toPairs rest
    toPairs _ = error "Internal error: invalid argument structure"

    checkValidFlags :: [(String, String)] -> IO ()
    checkValidFlags pairs = mapM_ checkPair pairs
      where
        valid = ["-i", "-f", "-o", "-e"]
        checkPair (f, _) | f `elem` valid = pure ()
                         | otherwise = usageError $ "Invalid flag: " ++ f

    checkRequiredFlags :: [(String, String)] -> IO ()
    checkRequiredFlags pairs =
        unless ("-i" `elem` flags) (usageError "Missing required flag: -i") *>
        unless ("-f" `elem` flags) (usageError "Missing required flag: -f")
      where flags = map fst pairs

    checkDuplicates :: [(String, String)] -> IO ()
    checkDuplicates pairs = case duplicates of
        [] -> pure ()
        ds -> usageError $ "Duplicate flags: " ++ unwords ds
      where
        flags = map fst pairs
        duplicates = flags \\ nub flags

    getRequired :: String -> [(String, String)] -> IO String
    getRequired flag pairs = maybe (usageError $ "Missing value for " ++ flag) pure (lookup flag pairs)

    getOptional :: String -> [(String, String)] -> IO (Maybe String)
    getOptional flag pairs = pure (lookup flag pairs)