{-
-- EPITECH PROJECT, 2024
-- JsonParser.hs
-- File description:
-- JSON Parser implementation for document conversion
-}

module JsonParser
    ( parseJson
    ) where

import ParsingLibrary
import Document
import Data.Char (isDigit)

-- | Our custom data types for JSON parsing
data JsonVal
    = JsonStr String
    | JsonNum Double
    | JsonBool Bool
    | JsonNull
    | JsonArr [JsonVal]
    | JsonObj [(String, JsonVal)]
    deriving (Show)

-- | Main entry point - Parse a full JSON document
parseJson :: Parser Document
parseJson = do
    skipWhitespace
    obj <- parseJsonObject
    skipWhitespace
    case jsonToDocument obj of
        Just doc -> return doc
        Nothing -> emptyP

-- | Parse a JSON object {key: value, ...}
parseJsonObject :: Parser [(String, JsonVal)]
parseJsonObject = do
    char '{'
    skipWhitespace
    pairs <- parseJsonPairs
    skipWhitespace
    char '}'
    return pairs

-- | Parse key-value pairs in a JSON object
parseJsonPairs :: Parser [(String, JsonVal)]
parseJsonPairs = do
    input <- currentInput
    if take 1 input == "}"
        then return []
        else parseJsonPairList

-- | Parse a list of key-value pairs
parseJsonPairList :: Parser [(String, JsonVal)]
parseJsonPairList = do
    pair <- parseJsonPair
    skipWhitespace
    input <- currentInput
    if take 1 input == ","
        then do
            char ','
            skipWhitespace
            rest <- parseJsonPairs
            return (pair : rest)
        else return [pair]

-- | Parse a single key-value pair
parseJsonPair :: Parser (String, JsonVal)
parseJsonPair = do
    skipWhitespace
    key <- parseJsonString
    skipWhitespace
    char ':'
    skipWhitespace
    value <- parseJsonValue
    return (key, value)

-- | Parse any JSON value
parseJsonValue :: Parser JsonVal
parseJsonValue = do
    skipWhitespace
    input <- currentInput
    case take 1 input of
        "\"" -> parseJsonString >>= return . JsonStr
        "[" -> parseJsonArray >>= return . JsonArr
        "{" -> parseJsonObject >>= return . JsonObj
        "t" -> parseJsonTrue
        "f" -> parseJsonFalse
        "n" -> parseJsonNull
        _ -> if isNumberStart (head input)
             then parseJsonNumber
             else emptyP

-- | Check if a character can start a number
isNumberStart :: Char -> Bool
isNumberStart c = isDigit c || c == '-' || c == '.'

-- | Parse a JSON string (with proper escaping)
parseJsonString :: Parser String
parseJsonString = do
    char '"'
    content <- parseJsonStringContent
    char '"'
    return content

-- | Parse the content of a JSON string with escape handling
parseJsonStringContent :: Parser String
parseJsonStringContent = Parser $ \input ->
    parseStringHelper "" input
  where
    parseStringHelper acc "" = Nothing
    parseStringHelper acc ('"':rest) = Just (reverse acc, '"':rest)
    parseStringHelper acc ('\\':'"':rest) = parseStringHelper ('"':acc) rest
    parseStringHelper acc ('\\':'\\':rest) = parseStringHelper ('\\':acc) rest
    parseStringHelper acc ('\\':'n':rest) = parseStringHelper ('\n':acc) rest
    parseStringHelper acc ('\\':'r':rest) = parseStringHelper ('\r':acc) rest
    parseStringHelper acc ('\\':'t':rest) = parseStringHelper ('\t':acc) rest
    parseStringHelper acc (c:rest) = parseStringHelper (c:acc) rest

-- | Parse a JSON array [value, value, ...]
parseJsonArray :: Parser [JsonVal]
parseJsonArray = do
    char '['
    skipWhitespace
    items <- parseJsonArrayItems
    skipWhitespace
    char ']'
    return items

-- | Parse items in a JSON array
parseJsonArrayItems :: Parser [JsonVal]
parseJsonArrayItems = do
    input <- currentInput
    if take 1 input == "]"
        then return []
        else parseJsonItemList

-- | Parse a list of array items
parseJsonItemList :: Parser [JsonVal]
parseJsonItemList = do
    item <- parseJsonValue
    skipWhitespace
    input <- currentInput
    if take 1 input == ","
        then do
            char ','
            skipWhitespace
            rest <- parseJsonArrayItems
            return (item : rest)
        else return [item]

-- | Parse JSON true value
parseJsonTrue :: Parser JsonVal
parseJsonTrue = do
    stringP "true"
    return (JsonBool True)

-- | Parse JSON false value
parseJsonFalse :: Parser JsonVal
parseJsonFalse = do
    stringP "false"
    return (JsonBool False)

-- | Parse JSON null value
parseJsonNull :: Parser JsonVal
parseJsonNull = do
    stringP "null"
    return JsonNull

-- | Parse a JSON number
parseJsonNumber :: Parser JsonVal
parseJsonNumber = Parser $ \input ->
    let (numStr, rest) = span isNumberChar input
    in if null numStr
       then Nothing
       else Just (JsonNum (read numStr), rest)

-- | Check if a character can be part of a number
isNumberChar :: Char -> Bool
isNumberChar c = isDigit c || c == '.' ||
                 c == '-' || c == '+' || c == 'e' || c == 'E'

-- | Convert parsed JSON to Document
jsonToDocument :: [(String, JsonVal)] -> Maybe Document
jsonToDocument obj = do
    header' <- extractHeader obj
    body' <- extractBody obj
    return (Document header' body')

-- | Extract header from JSON object
extractHeader :: [(String, JsonVal)] -> Maybe Header
extractHeader obj = do
    headerVal <- lookup "header" obj
    case headerVal of
        JsonObj fields -> do
            title' <- extractStringField "title" fields
            let author' = extractOptionalStringField "author" fields
            let date' = extractOptionalStringField "date" fields
            return (Header title' author' date')
        _ -> Nothing

-- | Extract body from JSON object
extractBody :: [(String, JsonVal)] -> Maybe [Content]
extractBody obj = do
    bodyVal <- lookup "body" obj
    case bodyVal of
        JsonArr elements -> mapM extractContent elements
        _ -> Nothing

-- | Extract a string field from JSON object
extractStringField :: String -> [(String, JsonVal)] -> Maybe String
extractStringField key fields = do
    value <- lookup key fields
    case value of
        JsonStr s -> Just s
        _ -> Nothing

-- | Extract an optional string field from JSON object
extractOptionalStringField :: String -> [(String, JsonVal)] -> Maybe String
extractOptionalStringField key fields =
    case lookup key fields of
        Just (JsonStr s) -> Just s
        _ -> Nothing

-- | Extract content from JSON value
extractContent :: JsonVal -> Maybe Content
extractContent (JsonStr s) = Just (Paragraph [PlainText s])
extractContent (JsonObj obj)
    | Just para <- extractParagraph obj = Just para
    | Just section <- extractSection obj = Just section
    | Just codeBlock <- extractCodeBlock obj = Just codeBlock
    | Just list <- extractList obj = Just list
    | otherwise = Nothing
extractContent _ = Nothing

-- | Extract paragraph from JSON object
extractParagraph :: [(String, JsonVal)] -> Maybe Content
extractParagraph obj = do
    paraVal <- lookup "paragraph" obj
    case paraVal of
        JsonArr inlines -> do
            items <- mapM extractInline inlines
            return (Paragraph items)
        _ -> Nothing

-- | Extract section from JSON object
extractSection :: [(String, JsonVal)] -> Maybe Content
extractSection obj = do
    sectionVal <- lookup "section" obj
    case sectionVal of
        JsonObj secObj -> do
            let title' = extractSectionTitle secObj
            contents' <- extractSectionContents secObj
            return (Section title' contents')
        _ -> Nothing

-- | Extract section title from object
extractSectionTitle :: [(String, JsonVal)] -> Maybe String
extractSectionTitle obj =
    case lookup "title" obj of
        Just (JsonStr t) -> Just t
        _ -> Nothing

-- | Extract section contents
extractSectionContents :: [(String, JsonVal)] -> Maybe [Content]
extractSectionContents obj = do
    contentsVal <- lookup "contents" obj
    case contentsVal of
        JsonArr elements -> mapM extractContent elements
        _ -> Nothing

-- | Extract code block from JSON object
extractCodeBlock :: [(String, JsonVal)] -> Maybe Content
extractCodeBlock obj = do
    codeVal <- lookup "codeblock" obj
    case codeVal of
        JsonStr code -> Just (CodeBlock code)
        _ -> Nothing

-- | Extract list from JSON object
extractList :: [(String, JsonVal)] -> Maybe Content
extractList obj = do
    listVal <- lookup "list" obj
    case listVal of
        JsonArr items -> do
            listItems <- mapM extractListItem items
            return (List listItems)
        _ -> Nothing

-- | Extract list item from JSON value
extractListItem :: JsonVal -> Maybe ListItem
extractListItem (JsonObj obj) = do
    itemVal <- lookup "item" obj
    case itemVal of
        JsonArr inlines -> do
            content' <- mapM extractInline inlines
            return (ListItem content')
        _ -> Nothing
extractListItem _ = Nothing

-- | Extract inline element from JSON value
extractInline :: JsonVal -> Maybe Inline
extractInline (JsonStr s) = Just (PlainText s)
extractInline (JsonObj obj)
    | Just bold <- extractBold obj = Just bold
    | Just italic <- extractItalic obj = Just italic
    | Just code <- extractCode obj = Just code
    | Just link <- extractLink obj = Just link
    | Just image <- extractImage obj = Just image
    | otherwise = Nothing
extractInline _ = Nothing

-- | Extract bold text from JSON object
extractBold :: [(String, JsonVal)] -> Maybe Inline
extractBold obj = do
    boldVal <- lookup "bold" obj
    case boldVal of
        JsonArr inlines -> do
            content' <- mapM extractInline inlines
            return (Bold content')
        _ -> Nothing

-- | Extract italic text from JSON object
extractItalic :: [(String, JsonVal)] -> Maybe Inline
extractItalic obj = do
    italicVal <- lookup "italic" obj
    case italicVal of
        JsonArr inlines -> do
            content' <- mapM extractInline inlines
            return (Italic content')
        _ -> Nothing

-- | Extract code from JSON object
extractCode :: [(String, JsonVal)] -> Maybe Inline
extractCode obj = do
    codeVal <- lookup "code" obj
    case codeVal of
        JsonStr code -> Just (Code code)
        _ -> Nothing

-- | Extract link from JSON object
extractLink :: [(String, JsonVal)] -> Maybe Inline
extractLink obj = do
    linkVal <- lookup "link" obj
    case linkVal of
        JsonObj linkObj -> do
            text <- extractStringField "text" linkObj
            url <- extractStringField "url" linkObj
            return (Link text url)
        _ -> Nothing

-- | Extract image from JSON object
extractImage :: [(String, JsonVal)] -> Maybe Inline
extractImage obj = do
    imageVal <- lookup "image" obj
    case imageVal of
        JsonObj imageObj -> do
            alt <- extractStringField "alt" imageObj
            url <- extractStringField "url" imageObj
            return (Image alt url)
        _ -> Nothing
