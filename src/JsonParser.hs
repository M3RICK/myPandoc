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

-- Type alias for simplicity
type JsonObject = [(String, JsonValue)]

data JsonValue
    = JsonString String
    | JsonArray [JsonValue]
    | JsonObject JsonObject
    deriving (Show)

-- | Parse a full JSON document into a Document
parseJson :: Parser Document
parseJson = Parser $ \input ->
    case run parseJsonObject (dropWhile isSpace input) of
        Nothing -> Nothing
        Just (obj, rest) -> case buildDocument obj of
            Just doc -> Just (doc, rest)
            Nothing -> Nothing

-- | Parse a JSON string (text surrounded by double quotes)
parseJsonString :: Parser String
parseJsonString = Parser $ \input ->
    case dropWhile isSpace input of
        ('"':rest) -> collectString "" rest
        _ -> Nothing
  where
    collectString :: String -> String -> Maybe (String, String)
    collectString acc ('"':xs) = Just (reverse acc, xs)
    collectString acc ('\\':'"':xs) = collectString ('\"':acc) xs
    collectString acc ('\\':'\\':xs) = collectString ('\\':acc) xs
    collectString acc (x:xs) = collectString (x:acc) xs
    collectString _ [] = Nothing

-- | Parse a JSON key (which is just a string)
parseJsonKey :: Parser String
parseJsonKey = parseJsonString

-- | Parse a JSON pair (key: value)
parseJsonPair :: Parser (String, JsonValue)
parseJsonPair = Parser $ \input ->
    case run parseJsonKey (dropWhile isSpace input) of
        Nothing -> Nothing
        Just (key, rest1) -> parseKeyValue key rest1

-- | Helper for parsing the value part of a key-value pair
parseKeyValue :: String -> String -> Maybe ((String, JsonValue), String)
parseKeyValue key rest1 =
    case dropWhile isSpace rest1 of
        (':':rest2) -> case run parseJsonValue (dropWhile isSpace rest2) of
            Nothing -> Nothing
            Just (val, rest3) -> Just ((key, val), rest3)
        _ -> Nothing

-- | Parse a JSON object ({ ... })
parseJsonObject :: Parser JsonObject
parseJsonObject = Parser $ \input ->
    case dropWhile isSpace input of
        ('{':rest) -> parseObjectContent [] rest
        _ -> Nothing

-- | Helper for parsing object contents
parseObjectContent :: JsonObject -> String -> Maybe (JsonObject, String)
parseObjectContent acc rest =
    case dropWhile isSpace rest of
        ('}':rest') -> Just (acc, rest')
        _ -> parsePairAndContinue acc rest

-- | Helper to parse a pair and continue parsing object
parsePairAndContinue :: JsonObject -> String -> Maybe (JsonObject, String)
parsePairAndContinue acc rest =
    case run parseJsonPair (dropWhile isSpace rest) of
        Nothing -> Nothing
        Just (pair, rest') -> parseObjectEnd acc pair rest'

-- | Helper to handle the end of an object or continue with more pairs
parseObjectEnd :: JsonObject -> (String, JsonValue) -> String -> Maybe (JsonObject, String)
parseObjectEnd acc pair rest' =
    case dropWhile isSpace rest' of
        ('}':rest'') -> Just (acc ++ [pair], rest'')
        (',':rest'') -> parseObjectContent (acc ++ [pair]) rest''
        _ -> Nothing

-- | Parse a JSON array ([ ... ])
parseJsonArray :: Parser [JsonValue]
parseJsonArray = Parser $ \input ->
    case dropWhile isSpace input of
        ('[':rest) -> parseArrayContent [] rest
        _ -> Nothing

-- | Helper for parsing array contents
parseArrayContent :: [JsonValue] -> String -> Maybe ([JsonValue], String)
parseArrayContent acc rest =
    case dropWhile isSpace rest of
        (']':rest') -> Just (acc, rest')
        _ -> parseValueAndContinue acc rest

-- | Helper to parse a value and continue parsing array
parseValueAndContinue :: [JsonValue] -> String -> Maybe ([JsonValue], String)
parseValueAndContinue acc rest =
    case run parseJsonValue (dropWhile isSpace rest) of
        Nothing -> Nothing
        Just (val, rest') -> parseArrayEnd acc val rest'

-- | Helper to handle the end of an array or continue with more values
parseArrayEnd :: [JsonValue] -> JsonValue -> String -> Maybe ([JsonValue], String)
parseArrayEnd acc val rest' =
    case dropWhile isSpace rest' of
        (']':rest'') -> Just (acc ++ [val], rest'')
        (',':rest'') -> parseArrayContent (acc ++ [val]) rest''
        _ -> Nothing

-- | Parse a JSON value (string, object, or array)
parseJsonValue :: Parser JsonValue
parseJsonValue = Parser $ \input ->
    let trimmed = dropWhile isSpace input in
    case run parseJsonString trimmed of
        Just (str, rest) -> Just (JsonString str, rest)
        Nothing -> tryParseArray trimmed

-- | Try to parse an array, if that fails try to parse an object
tryParseArray :: String -> Maybe (JsonValue, String)
tryParseArray input =
    case run parseJsonArray input of
        Just (arr, rest) -> Just (JsonArray arr, rest)
        Nothing -> tryParseObject input

-- | Try to parse an object
tryParseObject :: String -> Maybe (JsonValue, String)
tryParseObject input =
    case run parseJsonObject input of
        Just (obj, rest) -> Just (JsonObject obj, rest)
        Nothing -> Nothing

-- | Build a Document from a JSON object
buildDocument :: JsonObject -> Maybe Document
buildDocument obj = do
    hdrVal <- lookup "header" obj
    bodyVal <- lookup "body" obj
    hdr <- buildHeaderFromJson hdrVal
    body <- buildBodyFromJson bodyVal
    return (Document hdr body)

-- | Build a Header from a JSON value
buildHeaderFromJson :: JsonValue -> Maybe Header
buildHeaderFromJson (JsonObject fields) = do
    title' <- getFieldAsString "title" fields
    let author' = getFieldAsString "author" fields
    let date' = getFieldAsString "date" fields
    return $ Header title' author' date'
buildHeaderFromJson _ = Nothing

-- | Extract string field from object
getFieldAsString :: String -> JsonObject -> Maybe String
getFieldAsString key fields =
    case lookup key fields of
        Just (JsonString s) -> Just s
        _ -> Nothing

-- | Build Content list from a JSON value
buildBodyFromJson :: JsonValue -> Maybe [Content]
buildBodyFromJson (JsonArray elems) = mapM convertContent elems
  where
    convertContent :: JsonValue -> Maybe Content
    convertContent (JsonString s) = Just (Paragraph [PlainText s])
    convertContent (JsonObject o) = buildStructuredContent o
    convertContent _ = Nothing
buildBodyFromJson _ = Nothing

-- | Build a structured content element from a JSON object
buildStructuredContent :: JsonObject -> Maybe Content
buildStructuredContent fields
    | hasKeyWithString "codeblock" fields =
        Just (CodeBlock (getStringValue "codeblock" fields))
    | hasKeyWithArray "list" fields = buildList fields
    | hasKeyWithString "section" fields && hasKeyWithArray "content" fields =
        buildSectionWithTitle fields
    | hasKeyWithArray "section" fields = buildSectionWithoutTitle fields
    | hasKeyWithArray "paragraph" fields = buildParagraph fields
    | otherwise = Nothing

-- | Check if object has a key with string value
hasKeyWithString :: String -> JsonObject -> Bool
hasKeyWithString key fields =
    case lookup key fields of
        Just (JsonString _) -> True
        _ -> False

-- | Check if object has a key with array value
hasKeyWithArray :: String -> JsonObject -> Bool
hasKeyWithArray key fields =
    case lookup key fields of
        Just (JsonArray _) -> True
        _ -> False

-- | Get string value from object
getStringValue :: String -> JsonObject -> String
getStringValue key fields =
    case lookup key fields of
        Just (JsonString s) -> s
        _ -> ""

-- | Get array value from object
getArrayValue :: String -> JsonObject -> [JsonValue]
getArrayValue key fields =
    case lookup key fields of
        Just (JsonArray arr) -> arr
        _ -> []

-- | Build a list content
buildList :: JsonObject -> Maybe Content
buildList fields = do
    let items = getArrayValue "list" fields
    listItems <- mapM jsonListItem items
    Just (List listItems)

-- | Build a section with title
buildSectionWithTitle :: JsonObject -> Maybe Content
buildSectionWithTitle fields = do
    let title' = getStringValue "section" fields
    let contentArray = getArrayValue "content" fields
    sectionContents <- mapM convertSectionContent contentArray
    Just (Section (Just title') sectionContents)

-- | Build a section without title
buildSectionWithoutTitle :: JsonObject -> Maybe Content
buildSectionWithoutTitle fields = do
    let contentArray = getArrayValue "section" fields
    sectionContents <- mapM convertSectionContent contentArray
    Just (Section Nothing sectionContents)

-- | Build a paragraph content
buildParagraph :: JsonObject -> Maybe Content
buildParagraph fields = do
    let inlines = getArrayValue "paragraph" fields
    paragraphContent <- mapM convertInline inlines
    Just (Paragraph paragraphContent)

-- | Convert section content
convertSectionContent :: JsonValue -> Maybe Content
convertSectionContent (JsonString s) = Just (Paragraph [PlainText s])
convertSectionContent (JsonObject o) = buildStructuredContent o
convertSectionContent _ = Nothing

-- | Convert inline content
convertInline :: JsonValue -> Maybe Inline
convertInline (JsonString s) = Just (PlainText s)
convertInline _ = Nothing

-- | Convert a JSON value to a list item
jsonListItem :: JsonValue -> Maybe ListItem
jsonListItem (JsonString s) = Just (ListItem [PlainText s])
jsonListItem (JsonObject o)
    | hasKeyWithArray "content" o = do
        let contentArray = getArrayValue "content" o
        content <- mapM convertItemContent contentArray
        Just (ListItem content)
    | otherwise = Nothing
  where
    convertItemContent :: JsonValue -> Maybe Inline
    convertItemContent (JsonString s) = Just (PlainText s)
    convertItemContent _ = Nothing
jsonListItem _ = Nothing
