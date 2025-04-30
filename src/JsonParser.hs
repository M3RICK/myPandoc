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

-- | Our custom data types for JSON parsing
type JsonObj = [(String, JsonVal)]

data JsonVal
    = JsonStr String
    | JsonNum Double
    | JsonBool Bool
    | JsonNull
    | JsonArr [JsonVal]
    | JsonObj JsonObj
    deriving (Show)

-- | Main entry point - Parse a full JSON document
parseJson :: Parser Document
parseJson = 
    skipWhitespace *>
    parseJsonObject >>= \obj ->
    skipWhitespace *>
    case jsonToDocument obj of
        Just doc -> return doc
        Nothing -> emptyP

-- | Parse a JSON object {key: value, ...}
parseJsonObject :: Parser JsonObj
parseJsonObject = 
    skipWhitespace *>
    char '{' *>
    skipWhitespace *>
    parseJsonPairs >>= \result ->
    skipWhitespace *>
    char '}' *>
    return result

-- | Check if we've reached the end of object
isEndOfObject :: String -> Bool
isEndOfObject input = take 1 input == "}"

-- | Parse the first pair in an object
parseFirstPair :: Parser JsonObj
parseFirstPair =
    parseJsonPair >>= \pair ->
    handleRestOfPairs pair

-- | Handle the rest of the pairs after the first one
handleRestOfPairs :: (String, JsonVal) -> Parser JsonObj
handleRestOfPairs pair =
    skipWhitespace *>
    currentInput >>= \nextChar ->
    if take 1 nextChar == ","
        then char ',' *>
             parseJsonPairs >>= \rest ->
             return (pair : rest)
        else return [pair]

-- | Parse key-value pairs in a JSON object
parseJsonPairs :: Parser JsonObj
parseJsonPairs = 
    skipWhitespace *>
    currentInput >>= \firstChar ->
    if isEndOfObject firstChar
        then return []
        else parseFirstPair

-- | Parse a single key-value pair
parseJsonPair :: Parser (String, JsonVal)
parseJsonPair = 
    skipWhitespace *>
    parseJsonString >>= \key ->
    skipWhitespace *>
    char ':' *>
    skipWhitespace *>
    parseJsonValue >>= \value ->
    return (key, value)

-- | Check if a character can start a number
isNumberStart :: Char -> Bool
isNumberStart c = (c >= '0' && c <= '9') || c == '-' || c == '.'

-- | Handle string values
handleStringValue :: Parser JsonVal
handleStringValue = parseJsonString >>= \s -> return (JsonStr s)

-- | Handle array values
handleArrayValue :: Parser JsonVal
handleArrayValue = parseJsonArray >>= \arr -> return (JsonArr arr)

-- | Handle object values
handleObjectValue :: Parser JsonVal
handleObjectValue = parseJsonObject >>= \obj -> return (JsonObj obj)

-- | Handle number values
handleNumberValue :: String -> Parser JsonVal
handleNumberValue firstChar =
    if not (null firstChar) && isNumberStart (head firstChar)
        then parseJsonNumber
        else emptyP

-- | Parse any JSON value
parseJsonValue :: Parser JsonVal
parseJsonValue = 
    skipWhitespace *>
    currentInput >>= \firstChar ->
    case take 1 firstChar of
        "\"" -> handleStringValue
        "[" -> handleArrayValue
        "{" -> handleObjectValue
        "t" -> parseJsonTrue
        "f" -> parseJsonFalse
        "n" -> parseJsonNull
        _ -> handleNumberValue firstChar

-- | Parse a JSON string (with proper escaping)
parseJsonString :: Parser String
parseJsonString = 
    char '"' *>
    parseJsonStringContent >>= \content ->
    char '"' *>
    return content

-- | Parse the content of a JSON string
parseJsonStringContent :: Parser String
parseJsonStringContent = Parser $ \input ->
    parseStringHelper "" input
  where
    parseStringHelper _ "" = Nothing
    parseStringHelper acc ('"':rest) = Just (reverse acc, '"':rest)
    parseStringHelper acc ('\\':'"':rest) = parseStringHelper ('"':acc) rest
    parseStringHelper acc ('\\':'\\':rest) = parseStringHelper ('\\':acc) rest
    parseStringHelper acc ('\\':'n':rest) = parseStringHelper ('\n':acc) rest
    parseStringHelper acc ('\\':'r':rest) = parseStringHelper ('\r':acc) rest
    parseStringHelper acc ('\\':'t':rest) = parseStringHelper ('\t':acc) rest
    parseStringHelper acc (c:rest) = parseStringHelper (c:acc) rest

-- | Check if we've reached the end of array
isEndOfArray :: String -> Bool
isEndOfArray input = take 1 input == "]"

-- | Parse the first item in an array
parseFirstItem :: Parser [JsonVal]
parseFirstItem =
    parseJsonValue >>= \item ->
    handleRestOfItems item

-- | Handle the rest of the items after the first one
handleRestOfItems :: JsonVal -> Parser [JsonVal]
handleRestOfItems item =
    skipWhitespace *>
    currentInput >>= \nextChar ->
    if take 1 nextChar == ","
        then char ',' *>
             parseJsonArrayItems >>= \rest ->
             return (item : rest)
        else return [item]

-- | Parse a JSON array [value, value, ...]
parseJsonArray :: Parser [JsonVal]
parseJsonArray = 
    char '[' *>
    skipWhitespace *>
    parseJsonArrayItems >>= \result ->
    skipWhitespace *>
    char ']' *>
    return result

-- | Parse items in a JSON array
parseJsonArrayItems :: Parser [JsonVal]
parseJsonArrayItems = 
    skipWhitespace *>
    currentInput >>= \firstChar ->
    if isEndOfArray firstChar
        then return []
        else parseFirstItem

-- | Parse JSON true value
parseJsonTrue :: Parser JsonVal
parseJsonTrue = 
    stringP "true" *>
    return (JsonBool True)

-- | Parse JSON false value
parseJsonFalse :: Parser JsonVal
parseJsonFalse = 
    stringP "false" *>
    return (JsonBool False)

-- | Parse JSON null value
parseJsonNull :: Parser JsonVal
parseJsonNull = 
    stringP "null" *>
    return JsonNull

-- | Check if a character can be part of a number
isNumberChar :: Char -> Bool
isNumberChar c = (c >= '0' && c <= '9') || c == '.' || 
                 c == '-' || c == '+' || c == 'e' || c == 'E'

-- | Parse a JSON number
parseJsonNumber :: Parser JsonVal
parseJsonNumber = Parser $ \input ->
    let (numStr, rest) = span isNumberChar input
    in if null numStr 
       then Nothing 
       else Just (JsonNum (read numStr), rest)

-- | Convert parsed JSON to Document
jsonToDocument :: JsonObj -> Maybe Document
jsonToDocument obj = do
    header' <- extractHeader obj
    body' <- extractBody obj
    return (Document header' body')

-- | Extract header from JSON object
extractHeader :: JsonObj -> Maybe Header
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
extractBody :: JsonObj -> Maybe [Content]
extractBody obj = do
    bodyVal <- lookup "body" obj
    case bodyVal of
        JsonArr elements -> mapM extractContent elements
        _ -> Nothing

-- | Extract a string field from JSON object
extractStringField :: String -> JsonObj -> Maybe String
extractStringField key fields = do
    value <- lookup key fields
    case value of
        JsonStr s -> Just s
        _ -> Nothing

-- | Extract an optional string field from JSON object
extractOptionalStringField :: String -> JsonObj -> Maybe String
extractOptionalStringField key fields =
    case lookup key fields of
        Just (JsonStr s) -> Just s
        _ -> Nothing

-- | Extract section title from object
extractSectionTitle :: JsonObj -> Maybe String
extractSectionTitle obj =
    case lookup "title" obj of
        Just (JsonStr t) -> Just t
        _ -> Nothing

-- | Try to extract paragraph content
tryExtractParagraph :: JsonObj -> Maybe Content
tryExtractParagraph obj = extractParagraph obj

-- | Try to extract section content
tryExtractSection :: JsonObj -> Maybe Content
tryExtractSection obj = extractSection obj

-- | Try to extract code block content
tryExtractCodeBlock :: JsonObj -> Maybe Content
tryExtractCodeBlock obj = extractCodeBlock obj

-- | Try to extract list content
tryExtractList :: JsonObj -> Maybe Content
tryExtractList obj = extractList obj

-- | Extract content from JSON value
extractContent :: JsonVal -> Maybe Content
extractContent (JsonStr s) = Just (Paragraph [PlainText s])
extractContent (JsonObj obj)
    | Just para <- tryExtractParagraph obj = Just para
    | Just section <- tryExtractSection obj = Just section
    | Just codeBlock <- tryExtractCodeBlock obj = Just codeBlock
    | Just list <- tryExtractList obj = Just list
    | otherwise = Nothing
extractContent _ = Nothing

-- | Extract paragraph from JSON object
extractParagraph :: JsonObj -> Maybe Content
extractParagraph obj = do
    paraVal <- lookup "paragraph" obj
    case paraVal of
        JsonArr inlines -> do
            items <- mapM extractInline inlines
            return (Paragraph items)
        _ -> Nothing

-- | Extract section contents
extractSectionContents :: JsonObj -> Maybe [Content]
extractSectionContents obj = do
    contentsVal <- lookup "contents" obj
    case contentsVal of
        JsonArr elements -> mapM extractContent elements
        _ -> Nothing

-- | Extract section from JSON object
extractSection :: JsonObj -> Maybe Content
extractSection obj = do
    sectionVal <- lookup "section" obj
    case sectionVal of
        JsonObj secObj -> do
            let title' = extractSectionTitle secObj
            contents' <- extractSectionContents secObj
            return (Section title' contents')
        _ -> Nothing

-- | Extract code block from JSON object
extractCodeBlock :: JsonObj -> Maybe Content
extractCodeBlock obj = do
    codeVal <- lookup "codeblock" obj
    case codeVal of
        JsonStr code -> Just (CodeBlock code)
        _ -> Nothing

-- | Extract list from JSON object
extractList :: JsonObj -> Maybe Content
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

-- | Try to extract bold inline
tryExtractBold :: JsonObj -> Maybe Inline
tryExtractBold obj = extractBold obj

-- | Try to extract italic inline
tryExtractItalic :: JsonObj -> Maybe Inline
tryExtractItalic obj = extractItalic obj

-- | Try to extract code inline
tryExtractCode :: JsonObj -> Maybe Inline
tryExtractCode obj = extractCode obj

-- | Try to extract link inline
tryExtractLink :: JsonObj -> Maybe Inline
tryExtractLink obj = extractLink obj

-- | Try to extract image inline
tryExtractImage :: JsonObj -> Maybe Inline
tryExtractImage obj = extractImage obj

-- | Extract inline element from JSON value
extractInline :: JsonVal -> Maybe Inline
extractInline (JsonStr s) = Just (PlainText s)
extractInline (JsonObj obj)
    | Just bold <- tryExtractBold obj = Just bold
    | Just italic <- tryExtractItalic obj = Just italic
    | Just code <- tryExtractCode obj = Just code
    | Just link <- tryExtractLink obj = Just link
    | Just image <- tryExtractImage obj = Just image
    | otherwise = Nothing
extractInline _ = Nothing

-- | Extract bold text from JSON object
extractBold :: JsonObj -> Maybe Inline
extractBold obj = do
    boldVal <- lookup "bold" obj
    case boldVal of
        JsonArr inlines -> do
            content' <- mapM extractInline inlines
            return (Bold content')
        _ -> Nothing

-- | Extract italic text from JSON object
extractItalic :: JsonObj -> Maybe Inline
extractItalic obj = do
    italicVal <- lookup "italic" obj
    case italicVal of
        JsonArr inlines -> do
            content' <- mapM extractInline inlines
            return (Italic content')
        _ -> Nothing

-- | Extract code from JSON object
extractCode :: JsonObj -> Maybe Inline
extractCode obj = do
    codeVal <- lookup "code" obj
    case codeVal of
        JsonStr code -> Just (Code code)
        _ -> Nothing

-- | Extract link from JSON object
extractLink :: JsonObj -> Maybe Inline
extractLink obj = do
    linkVal <- lookup "link" obj
    case linkVal of
        JsonObj linkObj -> do
            text <- extractStringField "text" linkObj
            url <- extractStringField "url" linkObj
            return (Link text url)
        _ -> Nothing

-- | Extract image from JSON object
extractImage :: JsonObj -> Maybe Inline
extractImage obj = do
    imageVal <- lookup "image" obj
    case imageVal of
        JsonObj imageObj -> do
            alt <- extractStringField "alt" imageObj
            url <- extractStringField "url" imageObj
            return (Image alt url)
        _ -> Nothing