{-
-- EPITECH PROJECT, 2024
-- JsonParser.hs
-- File description:
-- JSON Parser implementation for school format
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
parseJson::Parser Document
parseJson = do
    skipWhitespace
    obj <- parseJsonObject
    skipWhitespace
    convertToDocument obj

-- | Convert JSON object to Document structure
convertToDocument::[(String, JsonVal)] -> Parser Document
convertToDocument obj =
    case extractDocument obj of
        Just doc -> return doc
        Nothing -> emptyP

-- | Extract school document format
extractDocument::[(String, JsonVal)] -> Maybe Document
extractDocument obj = do
    header' <- extractHeader obj
    body' <- extractBody obj
    return (Document header' body')

-- | Extract header in school format
extractHeader::[(String, JsonVal)] -> Maybe Header
extractHeader obj = do
    headerVal <- lookup "header" obj
    case headerVal of
        JsonObj fields -> extractHeaderFields fields
        _ -> Nothing

-- | Extract header fields
extractHeaderFields::[(String, JsonVal)] -> Maybe Header
extractHeaderFields fields = do
    title' <- extractStringField "title" fields
    author' <- extractStringField "author" fields
    date' <- extractStringField "date" fields
    return (Header title' (Just author') (Just date'))

-- | Extract body in school format
extractBody::[(String, JsonVal)] -> Maybe [Content]
extractBody obj = do
    bodyVal <- lookup "body" obj
    case bodyVal of
        JsonArr elements -> mapM extractContent elements
        _ -> Nothing

-- | Extract content in school format
extractContent::JsonVal -> Maybe Content
extractContent (JsonArr elements) =
    Just (Paragraph (mapArrayToInlines elements))
extractContent (JsonObj fields) =
    extractObjectContent fields
extractContent _ = Nothing

-- | Extract content from object fields
extractObjectContent::[(String, JsonVal)] -> Maybe Content
extractObjectContent fields
    | Just section <- extractSection fields = Just section
    | Just codeblock <- extractCodeblock fields = Just codeblock
    | Just list <- extractList fields = Just list
    | otherwise = Nothing

-- | Extract section in school format
extractSection::[(String, JsonVal)] -> Maybe Content
extractSection fields = do
    sectionVal <- lookup "section" fields
    extractSectionContent sectionVal

-- | Extract section content
extractSectionContent::JsonVal -> Maybe Content
extractSectionContent (JsonObj secFields) = do
    -- Get title (might be empty string)
    let title' = getTitleFromFields secFields

    -- Get content array
    contentVal <- lookup "content" secFields
    contents' <- extractContentArray contentVal
    return (Section title' contents')
extractSectionContent _ = Nothing

-- | Get title from section fields
getTitleFromFields::[(String, JsonVal)] -> Maybe String
getTitleFromFields fields =
    case lookup "title" fields of
        Just (JsonStr t) -> Just t
        _ -> Just ""

-- | Extract content array from JsonVal
extractContentArray::JsonVal -> Maybe [Content]
extractContentArray (JsonArr elements) = mapM extractContent elements
extractContentArray _ = Nothing

-- | Extract codeblock in school format
extractCodeblock::[(String, JsonVal)] -> Maybe Content
extractCodeblock fields = do
    codeVal <- lookup "codeblock" fields
    case codeVal of
        JsonArr elements ->
            let codeText = concatMap jsonValToString elements
            in Just (CodeBlock codeText)
        JsonStr code -> Just (CodeBlock code)
        _ -> Nothing

-- | Convert JsonVal to string
jsonValToString::JsonVal -> String
jsonValToString (JsonStr s) = s
jsonValToString (JsonNum n) = show n
jsonValToString (JsonBool True) = "true"
jsonValToString (JsonBool False) = "false"
jsonValToString JsonNull = "null"
jsonValToString (JsonArr arr) = concatMap jsonValToString arr
jsonValToString (JsonObj _) = ""

-- | Extract list in school format
extractList::[(String, JsonVal)] -> Maybe Content
extractList fields = do
    listVal <- lookup "list" fields
    extractListItems listVal

-- | Extract list items
extractListItems::JsonVal -> Maybe Content
extractListItems (JsonArr items) = do
    listItems <- mapM extractListItem items
    return (List listItems)
extractListItems _ = Nothing

-- | Extract list item in school format
extractListItem::JsonVal -> Maybe ListItem
extractListItem (JsonArr elements) =
    Just (ListItem (mapArrayToInlines elements))
extractListItem _ = Nothing

-- | Convert array of JSON values to array of Inlines
mapArrayToInlines::[JsonVal] -> [Inline]
mapArrayToInlines = foldr processJsonVal []
  where
    processJsonVal val acc =
      case extractInline val of
          Just inline -> inline : acc
          Nothing -> acc

-- | Extract inline element in school format
extractInline::JsonVal -> Maybe Inline
extractInline (JsonStr s) =
    Just (PlainText s)
extractInline (JsonObj fields)
    | Just bold <- extractBold fields = Just bold
    | Just italic <- extractItalic fields = Just italic
    | Just code <- extractCode fields = Just code
    | Just link <- extractLink fields = Just link
    | Just image <- extractImage fields = Just image
    | otherwise = Nothing
extractInline _ = Nothing

-- | Extract bold in school format
extractBold::[(String, JsonVal)] -> Maybe Inline
extractBold fields = extractFormattingInline "bold" Bold fields

-- | Extract italic in school format
extractItalic::[(String, JsonVal)] -> Maybe Inline
extractItalic fields = extractFormattingInline "italic" Italic fields

-- | Helper to extract formatting inline elements
extractFormattingInline::String -> ([Inline] -> Inline) -> [(String, JsonVal)] -> Maybe Inline
extractFormattingInline key constructor fields = do
    val <- lookup key fields
    case val of
        JsonStr text -> Just (constructor [PlainText text])
        JsonArr elements -> Just (constructor (mapArrayToInlines elements))
        _ -> Nothing

-- | Extract code in school format
extractCode::[(String, JsonVal)] -> Maybe Inline
extractCode fields = do
    codeVal <- lookup "code" fields
    case codeVal of
        JsonStr text -> Just (Code text)
        _ -> Nothing

-- | Extract link in school format
extractLink::[(String, JsonVal)] -> Maybe Inline
extractLink fields = do
    linkVal <- lookup "link" fields
    case linkVal of
        JsonObj linkFields -> extractLinkFields linkFields
        _ -> Nothing

-- | Extract link fields
extractLinkFields::[(String, JsonVal)] -> Maybe Inline
extractLinkFields linkFields = do
    url <- extractStringField "url" linkFields
    contentVal <- lookup "content" linkFields
    let text = extractLinkText contentVal
    return (Link text url)

-- | Extract link text from content
extractLinkText::JsonVal -> String
extractLinkText (JsonArr elements) = concatMap jsonValToString elements
extractLinkText (JsonStr text) = text
extractLinkText _ = ""

-- | Extract image in school format
extractImage::[(String, JsonVal)] -> Maybe Inline
extractImage fields = do
    imageVal <- lookup "image" fields
    case imageVal of
        JsonObj imageFields -> extractImageFields imageFields
        _ -> Nothing

-- | Extract image fields
extractImageFields::[(String, JsonVal)] -> Maybe Inline
extractImageFields imageFields = do
    url <- extractStringField "url" imageFields
    altVal <- lookup "alt" imageFields
    let alt = extractImageAlt altVal
    return (Image alt url)

-- | Extract image alt text
extractImageAlt::JsonVal -> String
extractImageAlt (JsonArr elements) = concatMap jsonValToString elements
extractImageAlt (JsonStr alt) = alt
extractImageAlt _ = ""

-- | Extract a string field from JSON object
extractStringField::String -> [(String, JsonVal)] -> Maybe String
extractStringField key fields = do
    value <- lookup key fields
    case value of
        JsonStr s -> Just s
        _ -> Nothing

-- | Parse a JSON object {key: value, ...}
parseJsonObject::Parser [(String, JsonVal)]
parseJsonObject = do
    char '{'
    skipWhitespace
    pairs <- parseJsonPairs
    skipWhitespace
    char '}'
    return pairs

-- | Parse key-value pairs in a JSON object
parseJsonPairs::Parser [(String, JsonVal)]
parseJsonPairs = do
    input <- currentInput
    if take 1 input == "}"
        then return []
        else parseJsonPairList

-- | Parse a list of key-value pairs
parseJsonPairList::Parser [(String, JsonVal)]
parseJsonPairList = do
    pair <- parseJsonPair
    parseRemainingPairs pair

-- | Parse remaining pairs after the first one
parseRemainingPairs::(String, JsonVal) -> Parser [(String, JsonVal)]
parseRemainingPairs pair = do
    skipWhitespace
    input <- currentInput
    handleRemainingPairs pair input

-- | Helper to handle remaining pairs based on input
handleRemainingPairs::(String, JsonVal) -> String -> Parser [(String, JsonVal)]
handleRemainingPairs pair input
    | null input = return [pair]
    | take 1 input == "," = parseNextPair pair
    | otherwise = return [pair]

-- | Parse the next pair after a comma
parseNextPair::(String, JsonVal) -> Parser [(String, JsonVal)]
parseNextPair pair = do
    char ','
    skipWhitespace
    rest <- parseJsonPairs
    return (pair : rest)

-- | Parse a single key-value pair
parseJsonPair::Parser (String, JsonVal)
parseJsonPair = do
    skipWhitespace
    key <- parseJsonString
    skipWhitespace
    char ':'
    skipWhitespace
    value <- parseJsonValue
    return (key, value)

-- | Parse any JSON value
parseJsonValue::Parser JsonVal
parseJsonValue = do
    skipWhitespace
    input <- currentInput
    if null input
        then emptyP
        else parseBasedOnFirstChar input

-- | Parse JSON value based on first character
parseBasedOnFirstChar::String -> Parser JsonVal
parseBasedOnFirstChar input =
    case take 1 input of
        "\"" -> fmap JsonStr parseJsonString
        "[" -> fmap JsonArr parseJsonArray
        "{" -> fmap JsonObj parseJsonObject
        "t" -> parseJsonTrue
        "f" -> parseJsonFalse
        "n" -> parseJsonNull
        _ -> parseNumberIfPossible input

-- | Parse a number if possible
parseNumberIfPossible::String -> Parser JsonVal
parseNumberIfPossible input =
    if canBeNumber input
        then parseJsonNumber
        else emptyP

-- | Check if input can be a number
canBeNumber::String -> Bool
canBeNumber [] = False
canBeNumber (c:_) = isNumberStart c

-- | Check if a character can start a number
isNumberStart::Char -> Bool
isNumberStart c = isDigit c || c == '-' || c == '.'

-- | Check if a character can be part of a number
isNumberChar::Char -> Bool
isNumberChar c = isDigit c || c == '.' ||
                 c == '-' || c == '+' || c == 'e' || c == 'E'

-- | Parse a JSON string with proper escaping
parseJsonString::Parser String
parseJsonString = do
    char '"'
    content <- parseJsonStringContent
    char '"'
    return content

-- | Parse the content of a JSON string
parseJsonStringContent::Parser String
parseJsonStringContent = Parser $ \input ->
    parseStringHelper "" input

-- | Helper function for parsing string content with escapes
parseStringHelper::String -> String -> ParseResult String
parseStringHelper acc "" = Nothing
parseStringHelper acc ('"':rest) = Just (reverse acc, '"':rest)
parseStringHelper acc ('\\':'"':rest) = parseStringHelper ('"':acc) rest
parseStringHelper acc ('\\':'\\':rest) = parseStringHelper ('\\':acc) rest
parseStringHelper acc ('\\':'n':rest) = parseStringHelper ('\n':acc) rest
parseStringHelper acc ('\\':'r':rest) = parseStringHelper ('\r':acc) rest
parseStringHelper acc ('\\':'t':rest) = parseStringHelper ('\t':acc) rest
parseStringHelper acc ('\\':c:rest) = parseStringHelper (c:acc) rest
parseStringHelper acc (c:rest) = parseStringHelper (c:acc) rest

-- | Parse a JSON array [value, value, ...]
parseJsonArray::Parser [JsonVal]
parseJsonArray = do
    char '['
    skipWhitespace
    items <- parseJsonArrayItems
    skipWhitespace
    char ']'
    return items

-- | Parse items in a JSON array
parseJsonArrayItems::Parser [JsonVal]
parseJsonArrayItems = do
    input <- currentInput
    if take 1 input == "]"
        then return []
        else parseJsonItemList

-- | Parse a list of array items
parseJsonItemList::Parser [JsonVal]
parseJsonItemList = do
    item <- parseJsonValue
    parseItemRest [item]

parseItemRest::[JsonVal] -> Parser [JsonVal]
parseItemRest items = do
    skipWhitespace
    input <- currentInput
    handleItemRest items input

-- | Helper to handle the rest of an array item based on input
handleItemRest::[JsonVal] -> String -> Parser [JsonVal]
handleItemRest items input
    | null input = return items
    | take 1 input == "," = parseNextItems items
    | otherwise = return items

-- | Parse the next items after a comma
parseNextItems::[JsonVal] -> Parser [JsonVal]
parseNextItems items = do
    char ','
    skipWhitespace
    item <- parseJsonValue
    parseItemRest (items ++ [item])

-- | Parse JSON true value
parseJsonTrue::Parser JsonVal
parseJsonTrue = stringP "true" >> return (JsonBool True)

-- | Parse JSON false value
parseJsonFalse::Parser JsonVal
parseJsonFalse = stringP "false" >> return (JsonBool False)

-- | Parse JSON null value
parseJsonNull::Parser JsonVal
parseJsonNull = stringP "null" >> return JsonNull

-- | Parse a JSON number
parseJsonNumber::Parser JsonVal
parseJsonNumber = Parser $ \input ->
    let (numStr, rest) = span isNumberChar input
    in if null numStr
       then Nothing
       else Just (JsonNum (safeRead numStr), rest)

-- | Safe read function for numbers
safeRead::String -> Double
safeRead str =
    case reads str of
        [(num, "")] -> num
        _ -> 0.0
