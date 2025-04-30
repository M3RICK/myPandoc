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
    convertToDocument obj

-- | Convert JSON object to Document
convertToDocument :: [(String, JsonVal)] -> Parser Document
convertToDocument obj =
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
    parseRemainingPairs pair

-- | Parse remaining pairs after the first one
parseRemainingPairs :: (String, JsonVal) -> Parser [(String, JsonVal)]
parseRemainingPairs pair = do
    skipWhitespace
    input <- currentInput
    if take 1 input == ","
        then parseNextPair pair
        else return [pair]

-- | Parse the next pair after a comma
parseNextPair :: (String, JsonVal) -> Parser [(String, JsonVal)]
parseNextPair pair = do
    char ','
    skipWhitespace
    rest <- parseJsonPairs
    return (pair : rest)

-- | Parse a single key-value pair
parseJsonPair :: Parser (String, JsonVal)
parseJsonPair = do
    skipWhitespace
    key <- parseJsonString
    parsePairValue key

-- | Parse the value part of a key-value pair
parsePairValue :: String -> Parser (String, JsonVal)
parsePairValue key = do
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
    parseBasedOnFirstChar input

-- | Parse JSON value based on first character
parseBasedOnFirstChar :: String -> Parser JsonVal
parseBasedOnFirstChar input =
    case take 1 input of
        "\"" -> parseJsonString >>= return . JsonStr
        "[" -> parseJsonArray >>= return . JsonArr
        "{" -> parseJsonObject >>= return . JsonObj
        "t" -> parseJsonTrue
        "f" -> parseJsonFalse
        "n" -> parseJsonNull
        _ -> parseNumberIfPossible input

-- | Parse a number if the input starts with a number
parseNumberIfPossible :: String -> Parser JsonVal
parseNumberIfPossible input =
    if not (null input) && isNumberStart (head input)
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

-- | Parse the content of a JSON string
parseJsonStringContent :: Parser String
parseJsonStringContent = Parser $ \input ->
    parseStringHelper "" input

-- | Helper function for parsing string content with escapes
parseStringHelper :: String -> String -> ParseResult String
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
    parseArrayEnd items

-- | Parse array end
parseArrayEnd :: [JsonVal] -> Parser [JsonVal]
parseArrayEnd items = do
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
    parseItemRest item

-- | Parse the rest of the items after the first one
parseItemRest :: JsonVal -> Parser [JsonVal]
parseItemRest item = do
    skipWhitespace
    input <- currentInput
    if take 1 input == ","
        then parseNextItem item
        else return [item]

-- | Parse the next item after a comma
parseNextItem :: JsonVal -> Parser [JsonVal]
parseNextItem item = do
    char ','
    skipWhitespace
    rest <- parseJsonArrayItems
    return (item : rest)

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
    extractHeaderFromValue headerVal

-- | Extract header from JSON value
extractHeaderFromValue :: JsonVal -> Maybe Header
extractHeaderFromValue (JsonObj fields) = do
    title' <- extractStringField "title" fields
    let author' = extractOptionalStringField "author" fields
    let date' = extractOptionalStringField "date" fields
    return (Header title' author' date')
extractHeaderFromValue _ = Nothing

-- | Extract body from JSON object
extractBody :: [(String, JsonVal)] -> Maybe [Content]
extractBody obj = do
    bodyVal <- lookup "body" obj
    extractBodyFromValue bodyVal

-- | Extract body from JSON value
extractBodyFromValue :: JsonVal -> Maybe [Content]
extractBodyFromValue (JsonArr elements) = mapM extractContent elements
extractBodyFromValue _ = Nothing

-- | Extract a string field from JSON object
extractStringField :: String -> [(String, JsonVal)] -> Maybe String
extractStringField key fields = do
    value <- lookup key fields
    extractStringFromValue value

-- | Extract string from JSON value
extractStringFromValue :: JsonVal -> Maybe String
extractStringFromValue (JsonStr s) = Just s
extractStringFromValue _ = Nothing

-- | Extract an optional string field from JSON object
extractOptionalStringField :: String -> [(String, JsonVal)] -> Maybe String
extractOptionalStringField key fields =
    case lookup key fields of
        Just (JsonStr s) -> Just s
        _ -> Nothing

-- | Extract content from JSON value
extractContent :: JsonVal -> Maybe Content
extractContent (JsonStr s) = Just (Paragraph [PlainText s])
extractContent (JsonObj obj) = extractContentFromObject obj
extractContent _ = Nothing

-- | Extract content from JSON object
extractContentFromObject :: [(String, JsonVal)] -> Maybe Content
extractContentFromObject obj
    | Just para <- extractParagraph obj = Just para
    | Just section <- extractSection obj = Just section
    | Just codeBlock <- extractCodeBlock obj = Just codeBlock
    | Just list <- extractList obj = Just list
    | otherwise = Nothing

-- | Extract paragraph from JSON object
extractParagraph :: [(String, JsonVal)] -> Maybe Content
extractParagraph obj = do
    paraVal <- lookup "paragraph" obj
    extractParagraphFromValue paraVal

-- | Extract paragraph from JSON value
extractParagraphFromValue :: JsonVal -> Maybe Content
extractParagraphFromValue (JsonArr inlines) = do
    items <- mapM extractInline inlines
    return (Paragraph items)
extractParagraphFromValue _ = Nothing

-- | Extract section from JSON object
extractSection :: [(String, JsonVal)] -> Maybe Content
extractSection obj = do
    sectionVal <- lookup "section" obj
    extractSectionFromValue sectionVal

-- | Extract section from JSON value
extractSectionFromValue :: JsonVal -> Maybe Content
extractSectionFromValue (JsonObj secObj) = do
    let title' = extractSectionTitle secObj
    contents' <- extractSectionContents secObj
    return (Section title' contents')
extractSectionFromValue _ = Nothing

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
    extractContentsFromValue contentsVal

-- | Extract contents from JSON value
extractContentsFromValue :: JsonVal -> Maybe [Content]
extractContentsFromValue (JsonArr elements) = mapM extractContent elements
extractContentsFromValue _ = Nothing

-- | Extract code block from JSON object
extractCodeBlock :: [(String, JsonVal)] -> Maybe Content
extractCodeBlock obj = do
    codeVal <- lookup "codeblock" obj
    extractCodeBlockFromValue codeVal

-- | Extract code block from JSON value
extractCodeBlockFromValue :: JsonVal -> Maybe Content
extractCodeBlockFromValue (JsonStr code) = Just (CodeBlock code)
extractCodeBlockFromValue _ = Nothing

-- | Extract list from JSON object
extractList :: [(String, JsonVal)] -> Maybe Content
extractList obj = do
    listVal <- lookup "list" obj
    extractListFromValue listVal

-- | Extract list from JSON value
extractListFromValue :: JsonVal -> Maybe Content
extractListFromValue (JsonArr items) = do
    listItems <- mapM extractListItem items
    return (List listItems)
extractListFromValue _ = Nothing

-- | Extract list item from JSON value
extractListItem :: JsonVal -> Maybe ListItem
extractListItem (JsonObj obj) = extractListItemFromObject obj
extractListItem _ = Nothing

-- | Extract list item from JSON object
extractListItemFromObject :: [(String, JsonVal)] -> Maybe ListItem
extractListItemFromObject obj = do
    itemVal <- lookup "item" obj
    extractItemFromValue itemVal

-- | Extract item from JSON value
extractItemFromValue :: JsonVal -> Maybe ListItem
extractItemFromValue (JsonArr inlines) = do
    content' <- mapM extractInline inlines
    return (ListItem content')
extractItemFromValue _ = Nothing

-- | Extract inline element from JSON value
extractInline :: JsonVal -> Maybe Inline
extractInline (JsonStr s) = Just (PlainText s)
extractInline (JsonObj obj) = extractInlineFromObject obj
extractInline _ = Nothing

-- | Extract inline from JSON object
extractInlineFromObject :: [(String, JsonVal)] -> Maybe Inline
extractInlineFromObject obj
    | Just bold <- extractBold obj = Just bold
    | Just italic <- extractItalic obj = Just italic
    | Just code <- extractCode obj = Just code
    | Just link <- extractLink obj = Just link
    | Just image <- extractImage obj = Just image
    | otherwise = Nothing

-- | Extract bold text from JSON object
extractBold :: [(String, JsonVal)] -> Maybe Inline
extractBold obj = do
    boldVal <- lookup "bold" obj
    extractBoldFromValue boldVal

-- | Extract bold from JSON value
extractBoldFromValue :: JsonVal -> Maybe Inline
extractBoldFromValue (JsonArr inlines) = do
    content' <- mapM extractInline inlines
    return (Bold content')
extractBoldFromValue _ = Nothing

-- | Extract italic text from JSON object
extractItalic :: [(String, JsonVal)] -> Maybe Inline
extractItalic obj = do
    italicVal <- lookup "italic" obj
    extractItalicFromValue italicVal

-- | Extract italic from JSON value
extractItalicFromValue :: JsonVal -> Maybe Inline
extractItalicFromValue (JsonArr inlines) = do
    content' <- mapM extractInline inlines
    return (Italic content')
extractItalicFromValue _ = Nothing

-- | Extract code from JSON object
extractCode :: [(String, JsonVal)] -> Maybe Inline
extractCode obj = do
    codeVal <- lookup "code" obj
    extractCodeFromValue codeVal

-- | Extract code from JSON value
extractCodeFromValue :: JsonVal -> Maybe Inline
extractCodeFromValue (JsonStr code) = Just (Code code)
extractCodeFromValue _ = Nothing

-- | Extract link from JSON object
extractLink :: [(String, JsonVal)] -> Maybe Inline
extractLink obj = do
    linkVal <- lookup "link" obj
    extractLinkFromValue linkVal

-- | Extract link from JSON value
extractLinkFromValue :: JsonVal -> Maybe Inline
extractLinkFromValue (JsonObj linkObj) = do
    text <- extractStringField "text" linkObj
    url <- extractStringField "url" linkObj
    return (Link text url)
extractLinkFromValue _ = Nothing

-- | Extract image from JSON object
extractImage :: [(String, JsonVal)] -> Maybe Inline
extractImage obj = do
    imageVal <- lookup "image" obj
    extractImageFromValue imageVal

-- | Extract image from JSON value
extractImageFromValue :: JsonVal -> Maybe Inline
extractImageFromValue (JsonObj imageObj) = do
    alt <- extractStringField "alt" imageObj
    url <- extractStringField "url" imageObj
    return (Image alt url)
extractImageFromValue _ = Nothing
