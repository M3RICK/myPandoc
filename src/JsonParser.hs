{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- JSON Parser implementation for document conversion
-}

module JsonParser
    ( parseJson
    ) where

import ParsingLibrary
import Document
import Data.Char (isSpace)

-- | Parse a full JSON document into a Document
parseJson :: Parser Document
parseJson = Parser $ \input ->
    case run parseJsonObject (dropWhile isSpace input) of
        Nothing -> Nothing
        Just (obj, rest) -> case buildDocument obj of
            Just doc -> Just (doc, rest)
            Nothing -> Nothing

-- Type alias for simplicity
type JsonObject = [(String, JsonValue)]

data JsonValue
    = JsonString String
    | JsonArray [JsonValue]
    | JsonObject JsonObject
    deriving (Show)

-- ------------------------
-- Basic JSON parsing
-- ------------------------

parseJsonString :: Parser String
parseJsonString = Parser $ \input -> case dropWhile isSpace input of
    ('"':rest) -> Just (go "" rest)
    _ -> Nothing
  where
    go acc ('"':xs) = (reverse acc, xs)
    go acc (x:xs) = go (x:acc) xs
    go _ [] = ("", []) -- Unterminated string (for now, be permissive)

parseJsonKey :: Parser String
parseJsonKey = parseJsonString

parseJsonValue :: Parser JsonValue
parseJsonValue = Parser $ \input ->
    case run parseJsonString input of
        Just (str, rest) -> Just (JsonString str, rest)
        Nothing -> case run parseJsonArray input of
            Just (arr, rest) -> Just (JsonArray arr, rest)
            Nothing -> case run parseJsonObject input of
                Just (obj, rest) -> Just (JsonObject obj, rest)
                Nothing -> Nothing

parseJsonPair :: Parser (String, JsonValue)
parseJsonPair = thenP parseJsonKey $ \key rest1 ->
    case run (skipWhitespace *> char ':' *> skipWhitespace *> parseJsonValue) rest1 of
        Nothing -> Nothing
        Just (val, rest2) -> Just ((key, val), rest2)

parseJsonObject :: Parser JsonObject
parseJsonObject = Parser $ \input -> case dropWhile isSpace input of
    ('{':rest) -> go [] rest
    _ -> Nothing
  where
    go acc rest = case run (skipWhitespace *> parseJsonPair) rest of
        Just (pair, rest') -> case run (skipWhitespace *> optionalComma) rest' of
            Just (_, rest'') -> go (acc ++ [pair]) rest''
            Nothing -> Nothing
        Nothing -> case dropWhile isSpace rest of
            ('}':rest') -> Just (acc, rest')
            _ -> Nothing

optionalComma :: Parser ()
optionalComma = orElse (char ',' *> pureP ()) (pureP ())

parseJsonArray :: Parser [JsonValue]
parseJsonArray = Parser $ \input -> case dropWhile isSpace input of
    ('[':rest) -> go [] rest
    _ -> Nothing
  where
    go acc rest = case run (skipWhitespace *> parseJsonValue) rest of
        Just (val, rest') -> case run (skipWhitespace *> optionalComma) rest' of
            Just (_, rest'') -> go (acc ++ [val]) rest''
            Nothing -> Nothing
        Nothing -> case dropWhile isSpace rest of
            (']':rest') -> Just (acc, rest')
            _ -> Nothing

-- ------------------------
-- Transform parsed JSON into Document
-- ------------------------

buildDocument :: JsonObject -> Maybe Document
buildDocument obj = do
    hdrVal <- lookup "header" obj
    bodyVal <- lookup "body" obj
    hdr <- buildHeaderFromJson hdrVal
    body <- buildBodyFromJson bodyVal
    return (Document hdr body)

buildHeaderFromJson :: JsonValue -> Maybe Header
buildHeaderFromJson (JsonObject fields) = do
    let getField key = case lookup key fields of
            Just (JsonString s) -> Just s
            _ -> Nothing
    let title' = getField "title"
    let author' = getField "author"
    let date' = getField "date"
    case title' of
        Just t -> Just $ Header t author' date'
        Nothing -> Nothing
buildHeaderFromJson _ = Nothing

buildBodyFromJson :: JsonValue -> Maybe [Content]
buildBodyFromJson (JsonArray elems) = mapM convertContent elems
  where
    convertContent (JsonString s) = Just (Paragraph [PlainText s])
    convertContent (JsonObject o) = buildStructuredContent o
    convertContent _ = Nothing
buildBodyFromJson _ = Nothing

buildStructuredContent :: JsonObject -> Maybe Content
buildStructuredContent fields
    | Just (JsonString s) <- lookup "codeblock" fields = Just (CodeBlock s)
    | Just (JsonArray items) <- lookup "list" fields = do
        listItems <- mapM jsonListItem items
        Just (List listItems)
    | otherwise = Nothing

jsonListItem :: JsonValue -> Maybe ListItem
jsonListItem (JsonString s) = Just (ListItem [PlainText s])
jsonListItem _ = Nothing
