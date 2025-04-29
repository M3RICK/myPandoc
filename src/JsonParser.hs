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

-- | Our custom data types for JSON parsing
type JsonObj = [(String, JsonVal)]

data JsonVal
    = JsonStr String
    | JsonArr [JsonVal]
    | JsonObj JsonObj
    deriving (Show)

-- | Main entry point - Parse a full JSON document
parseJson::Parser Document
parseJson = Parser $ \rawInput ->
    let input = dropWhile isSpace rawInput in
    case run findJsonObject input of
        Nothing -> Nothing
        Just (obj, rest) -> convertToDoc obj rest

-- | Convert JSON to document or return Nothing
convertToDoc::JsonObj -> String -> Maybe (Document, String)
convertToDoc obj rest =
    case jsonToDocument obj of
        Just doc -> Just (doc, rest)
        Nothing -> Nothing

-- | Grab a string from inside quotes
grabJsonString::Parser String
grabJsonString = Parser $ \input ->
    let cleaned = dropWhile isSpace input in
    case cleaned of
        ('"':chars) -> extractStringContent "" chars
        _ -> Nothing

-- | Extract string content between quotes
extractStringContent::String -> String -> Maybe (String, String)
extractStringContent collected ('"':rest) =
    Just (reverse collected, rest)
extractStringContent collected ('\\':'"':rest) =
    extractStringContent ('\"':collected) rest
extractStringContent collected ('\\':'\\':rest) =
    extractStringContent ('\\':collected) rest
extractStringContent collected (char:rest) =
    extractStringContent (char:collected) rest
extractStringContent _ [] = Nothing

-- | Object keys are just strings
getObjectKey::Parser String
getObjectKey = grabJsonString

-- | Parse JSON objects {key: value, ...}
findJsonObject::Parser JsonObj
findJsonObject = Parser $ \input ->
    case dropWhile isSpace input of
        ('{':afterBrace) -> collectObjectPairs [] afterBrace
        _ -> Nothing

-- | Helper to collect key-value pairs in objects
collectObjectPairs::JsonObj -> String -> Maybe (JsonObj, String)
collectObjectPairs pairs input =
    case dropWhile isSpace input of
        ('}':rest) -> Just (pairs, rest)
        _ -> parsePairAndContinue pairs input

-- | Parse a pair and continue object parsing
parsePairAndContinue::JsonObj -> String -> Maybe (JsonObj, String)
parsePairAndContinue pairs input =
    case run getKeyValuePair input of
        Nothing -> Nothing
        Just (pair, afterPair) -> handleAfterPair pairs pair afterPair

-- | Handle what comes after a key-value pair
handleAfterPair::JsonObj -> (String, JsonVal) -> String -> Maybe (JsonObj, String)
handleAfterPair pairs pair afterPair =
    case dropWhile isSpace afterPair of
        ('}':rest) -> Just (pairs ++ [pair], rest)
        (',':moreStuff) -> collectObjectPairs (pairs ++ [pair]) moreStuff
        _ -> Nothing

-- | Parse a key:value pair
getKeyValuePair::Parser (String, JsonVal)
getKeyValuePair = Parser $ \input ->
    case run getObjectKey (dropWhile isSpace input) of
        Nothing -> Nothing
        Just (key, afterKey) -> parseKeyValueRest key afterKey

-- | Parse the rest of a key:value pair after the key
parseKeyValueRest::String -> String -> Maybe ((String, JsonVal), String)
parseKeyValueRest key afterKey =
    case dropWhile isSpace afterKey of
        (':':afterColon) -> parseAfterColon key afterColon
        _ -> Nothing

-- | Parse what comes after the colon in a key:value pair
parseAfterColon::String -> String -> Maybe ((String, JsonVal), String)
parseAfterColon key afterColon =
    case run parseJsonValue (dropWhile isSpace afterColon) of
        Nothing -> Nothing
        Just (value, afterValue) -> Just ((key, value), afterValue)

-- | Parse JSON arrays
grabJsonArray::Parser [JsonVal]
grabJsonArray = Parser $ \input ->
    case dropWhile isSpace input of
        ('[':afterBracket) -> collectArrayItems [] afterBracket
        _ -> Nothing

-- | Collect items in a JSON array
collectArrayItems::[JsonVal] -> String -> Maybe ([JsonVal], String)
collectArrayItems items input =
    case dropWhile isSpace input of
        (']':rest) -> Just (items, rest)
        _ -> parseItemAndContinue items input

-- | Parse an item and continue array parsing
parseItemAndContinue::[JsonVal] -> String -> Maybe ([JsonVal], String)
parseItemAndContinue items input =
    case run parseJsonValue input of
        Nothing -> Nothing
        Just (value, afterValue) -> handleAfterValue items value afterValue

-- | Handle what comes after a value in an array
handleAfterValue::[JsonVal] -> JsonVal -> String -> Maybe ([JsonVal], String)
handleAfterValue items value afterValue =
    case dropWhile isSpace afterValue of
        (']':rest) -> Just (items ++ [value], rest)
        (',':moreStuff) -> collectArrayItems (items ++ [value]) moreStuff
        _ -> Nothing

-- | Parse any JSON value - the main recursive parser
parseJsonValue::Parser JsonVal
parseJsonValue = Parser $ \input ->
    let cleaned = dropWhile isSpace input in
    case run grabJsonString cleaned of
        Just (str, rest) -> Just (JsonStr str, rest)
        Nothing -> tryParseArray cleaned

-- | Try to parse an array, or fall back to object
tryParseArray::String -> Maybe (JsonVal, String)
tryParseArray input =
    case run grabJsonArray input of
        Just (arr, rest) -> Just (JsonArr arr, rest)
        Nothing -> tryParseObject input

-- | Try to parse an object
tryParseObject::String -> Maybe (JsonVal, String)
tryParseObject input =
    case run findJsonObject input of
        Just (obj, rest) -> Just (JsonObj obj, rest)
        Nothing -> Nothing

-- | Convert parsed JSON to Document
jsonToDocument::JsonObj -> Maybe Document
jsonToDocument obj = do
    hdrVal <- lookup "header" obj
    bodyVal <- lookup "body" obj
    hdr <- extractHeader hdrVal
    body <- extractBody bodyVal
    return (Document hdr body)

-- | Extract header from JSON
extractHeader::JsonVal -> Maybe Header
extractHeader (JsonObj fields) = do
    titleVal <- lookup "title" fields
    title' <- extractString titleVal
    let author' = lookup "author" fields >>= extractString
    let date' = lookup "date" fields >>= extractString
    return $ Header title' author' date'
extractHeader _ = Nothing

-- | Extract string from JSON value
extractString::JsonVal -> Maybe String
extractString (JsonStr s) = Just s
extractString _ = Nothing

-- | Extract document body from JSON
extractBody::JsonVal -> Maybe [Content]
extractBody (JsonArr elements) = mapM makeContent elements
  where
    makeContent::JsonVal -> Maybe Content
    makeContent (JsonStr s) = Just (Paragraph [PlainText s])
    makeContent (JsonObj o) = extractStructuredContent o
    makeContent _ = Nothing
extractBody _ = Nothing

-- | Check if object has a code block
hasCodeBlock::JsonObj -> Bool
hasCodeBlock obj =
    case lookup "codeblock" obj of
        Just (JsonStr _) -> True
        _ -> False

-- | Check if object has a list
hasList::JsonObj -> Bool
hasList obj =
    case lookup "list" obj of
        Just (JsonArr _) -> True
        _ -> False

-- | Check if object has a titled section
hasTitledSection::JsonObj -> Bool
hasTitledSection obj =
    case (lookup "section" obj, lookup "content" obj) of
        (Just (JsonStr _), Just (JsonArr _)) -> True
        _ -> False

-- | Check if object has an untitled section
hasUntitledSection::JsonObj -> Bool
hasUntitledSection obj =
    case lookup "section" obj of
        Just (JsonArr _) -> True
        _ -> False

-- | Check if object has a paragraph
hasParagraph::JsonObj -> Bool
hasParagraph obj =
    case lookup "paragraph" obj of
        Just (JsonArr _) -> True
        _ -> False

-- | Convert JSON object to structured content
extractStructuredContent::JsonObj -> Maybe Content
extractStructuredContent obj
    | hasCodeBlock obj = extractCodeBlock obj
    | hasList obj = extractList obj
    | hasTitledSection obj = extractTitledSection obj
    | hasUntitledSection obj = extractUntitledSection obj
    | hasParagraph obj = extractParagraph obj
    | otherwise = Nothing

-- | Extract a code block from JSON
extractCodeBlock::JsonObj -> Maybe Content
extractCodeBlock obj =
    case lookup "codeblock" obj of
        Just (JsonStr code) -> Just (CodeBlock code)
        _ -> Nothing

-- | Extract a list from JSON
extractList::JsonObj -> Maybe Content
extractList obj =
    case lookup "list" obj of
        Just (JsonArr items) -> do
            listItems <- mapM makeListItem items
            return (List listItems)
        _ -> Nothing

-- | Extract a titled section from JSON
extractTitledSection::JsonObj -> Maybe Content
extractTitledSection obj =
    case (lookup "section" obj, lookup "content" obj) of
        (Just (JsonStr title'), Just (JsonArr contents)) -> do
            sectionContents <- mapM makeSecContent contents
            return (Section (Just title') sectionContents)
        _ -> Nothing

-- | Extract an untitled section from JSON
extractUntitledSection::JsonObj -> Maybe Content
extractUntitledSection obj =
    case lookup "section" obj of
        Just (JsonArr contents) -> do
            sectionContents <- mapM makeSecContent contents
            return (Section Nothing sectionContents)
        _ -> Nothing

-- | Extract a paragraph from JSON
extractParagraph::JsonObj -> Maybe Content
extractParagraph obj =
    case lookup "paragraph" obj of
        Just (JsonArr inlines) -> do
            paragraphContents <- mapM extractPlainText inlines
            return (Paragraph paragraphContents)
        _ -> Nothing

-- | Convert JSON to section content
makeSecContent::JsonVal -> Maybe Content
makeSecContent (JsonStr s) = Just (Paragraph [PlainText s])
makeSecContent (JsonObj o) = extractStructuredContent o
makeSecContent _ = Nothing

-- | Extract plain text from JSON
extractPlainText::JsonVal -> Maybe Inline
extractPlainText (JsonStr s) = Just (PlainText s)
extractPlainText _ = Nothing

-- | Create a simple ListItem from string
makeSimpleListItem::String -> ListItem
makeSimpleListItem s = ListItem [PlainText s]

-- | Extract list item content
extractItemContent::JsonVal -> Maybe Inline
extractItemContent (JsonStr s) = Just (PlainText s)
extractItemContent _ = Nothing

-- | Create a ListItem from JSON with content
makeListItemWithContent::[JsonVal] -> Maybe ListItem
makeListItemWithContent inlines = do
    content <- mapM extractItemContent inlines
    return (ListItem content)

-- | Create a ListItem from JSON
makeListItem::JsonVal -> Maybe ListItem
makeListItem (JsonStr s) = Just (makeSimpleListItem s)
makeListItem (JsonObj o) =
    case lookup "content" o of
        Just (JsonArr inlines) -> makeListItemWithContent inlines
        _ -> Nothing
makeListItem _ = Nothing
