{-
-- EPITECH PROJECT, 2024
-- JsonParser.hs
-- File description:
-- JSON Parser implementation for school format - Fixed compilation error
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

-- | Convert JSON object to Document structure
convertToDocument :: [(String, JsonVal)] -> Parser Document
convertToDocument obj =
    case extractSchoolDocument obj of
        Just doc -> return doc
        Nothing -> emptyP  -- Parser fails if structure doesn't match

-- | Extract school document format
extractSchoolDocument :: [(String, JsonVal)] -> Maybe Document
extractSchoolDocument obj = do
    header' <- extractSchoolHeader obj
    body' <- extractSchoolBody obj
    return (Document header' body')

-- | Extract header in school format
extractSchoolHeader :: [(String, JsonVal)] -> Maybe Header
extractSchoolHeader obj = do
    headerVal <- lookup "header" obj
    case headerVal of
        JsonObj fields -> do
            title' <- extractStringField "title" fields
            author' <- extractStringField "author" fields
            date' <- extractStringField "date" fields
            return (Header title' (Just author') (Just date'))
        _ -> Nothing

-- | Extract body in school format
extractSchoolBody :: [(String, JsonVal)] -> Maybe [Content]
extractSchoolBody obj = do
    bodyVal <- lookup "body" obj
    case bodyVal of
        JsonArr elements -> mapM extractSchoolContent elements
        _ -> Nothing

-- | Extract content in school format
extractSchoolContent :: JsonVal -> Maybe Content
extractSchoolContent (JsonArr elements) =
    -- Array of elements is a paragraph
    Just (Paragraph (mapArrayToInlines elements))
extractSchoolContent (JsonObj fields) =
    -- Object can be section, codeblock, or list
    extractObjectContent fields
extractSchoolContent _ = Nothing

-- | Extract content from object fields
extractObjectContent :: [(String, JsonVal)] -> Maybe Content
extractObjectContent fields
    | Just section <- extractSchoolSection fields = Just section
    | Just codeblock <- extractSchoolCodeblock fields = Just codeblock
    | Just list <- extractSchoolList fields = Just list
    | otherwise = Nothing

-- | Extract section in school format
extractSchoolSection :: [(String, JsonVal)] -> Maybe Content
extractSchoolSection fields = do
    sectionVal <- lookup "section" fields
    case sectionVal of
        JsonObj secFields -> do
            -- Get title (might be empty string)
            let title' = case lookup "title" secFields of
                            Just (JsonStr t) -> Just t
                            _ -> Just ""

            -- Get content array
            contentVal <- lookup "content" secFields
            case contentVal of
                JsonArr elements -> do
                    contents' <- mapM extractSchoolContent elements
                    return (Section title' contents')
                _ -> Nothing
        _ -> Nothing

-- | Extract codeblock in school format
extractSchoolCodeblock :: [(String, JsonVal)] -> Maybe Content
extractSchoolCodeblock fields = do
    codeVal <- lookup "codeblock" fields
    case codeVal of
        JsonArr elements -> do
            -- Join array elements into a string
            let codeText = concatMap jsonValToString elements
            return (CodeBlock codeText)
        JsonStr code ->
            -- Direct string
            return (CodeBlock code)
        _ -> Nothing

-- | Convert JsonVal to string
jsonValToString :: JsonVal -> String
jsonValToString (JsonStr s) = s
jsonValToString (JsonNum n) = show n
jsonValToString (JsonBool True) = "true"
jsonValToString (JsonBool False) = "false"
jsonValToString JsonNull = "null"
jsonValToString (JsonArr arr) =
    concatMap jsonValToString arr
jsonValToString (JsonObj _) = ""  -- Skip objects

-- | Extract list in school format
extractSchoolList :: [(String, JsonVal)] -> Maybe Content
extractSchoolList fields = do
    listVal <- lookup "list" fields
    case listVal of
        JsonArr items -> do
            -- Convert each item to ListItem
            listItems <- mapM extractSchoolListItem items
            return (List listItems)
        _ -> Nothing

-- | Extract list item in school format
extractSchoolListItem :: JsonVal -> Maybe ListItem
extractSchoolListItem (JsonArr elements) =
    -- Array elements are inlines
    Just (ListItem (mapArrayToInlines elements))
extractSchoolListItem _ = Nothing

-- | Convert array of JSON values to array of Inlines
mapArrayToInlines :: [JsonVal] -> [Inline]
mapArrayToInlines [] = []
mapArrayToInlines (val:rest) =
    case extractSchoolInline val of
        Just inline -> inline : mapArrayToInlines rest
        Nothing -> mapArrayToInlines rest

-- | Extract inline element in school format
extractSchoolInline :: JsonVal -> Maybe Inline
extractSchoolInline (JsonStr s) =
    Just (PlainText s)
extractSchoolInline (JsonObj fields)
    | Just bold <- extractSchoolBold fields = Just bold
    | Just italic <- extractSchoolItalic fields = Just italic
    | Just code <- extractSchoolCode fields = Just code
    | Just link <- extractSchoolLink fields = Just link
    | Just image <- extractSchoolImage fields = Just image
    | otherwise = Nothing
extractSchoolInline _ = Nothing

-- | Extract bold in school format
extractSchoolBold :: [(String, JsonVal)] -> Maybe Inline
extractSchoolBold fields = do
    boldVal <- lookup "bold" fields
    case boldVal of
        JsonStr text ->
            Just (Bold [PlainText text])
        JsonArr elements ->
            Just (Bold (mapArrayToInlines elements))
        _ -> Nothing

-- | Extract italic in school format
extractSchoolItalic :: [(String, JsonVal)] -> Maybe Inline
extractSchoolItalic fields = do
    italicVal <- lookup "italic" fields
    case italicVal of
        JsonStr text ->
            Just (Italic [PlainText text])
        JsonArr elements ->
            Just (Italic (mapArrayToInlines elements))
        _ -> Nothing

-- | Extract code in school format
extractSchoolCode :: [(String, JsonVal)] -> Maybe Inline
extractSchoolCode fields = do
    codeVal <- lookup "code" fields
    case codeVal of
        JsonStr text -> Just (Code text)
        _ -> Nothing

-- | Extract link in school format
extractSchoolLink :: [(String, JsonVal)] -> Maybe Inline
extractSchoolLink fields = do
    linkVal <- lookup "link" fields
    case linkVal of
        JsonObj linkFields -> do
            url <- extractStringField "url" linkFields
            contentVal <- lookup "content" linkFields
            case contentVal of
                JsonArr elements ->
                    -- Join array elements into text
                    let text = concatMap jsonValToString elements
                    in Just (Link text url)
                JsonStr text ->
                    Just (Link text url)
                _ -> Nothing
        _ -> Nothing

-- | Extract image in school format
extractSchoolImage :: [(String, JsonVal)] -> Maybe Inline
extractSchoolImage fields = do
    imageVal <- lookup "image" fields
    case imageVal of
        JsonObj imageFields -> do
            url <- extractStringField "url" imageFields
            altVal <- lookup "alt" imageFields
            case altVal of
                JsonArr elements ->
                    -- Join array elements into alt text
                    let alt = concatMap jsonValToString elements
                    in Just (Image alt url)
                JsonStr alt ->
                    Just (Image alt url)
                _ -> Nothing
        _ -> Nothing

-- | Extract a string field from JSON object
extractStringField :: String -> [(String, JsonVal)] -> Maybe String
extractStringField key fields = do
    value <- lookup key fields
    case value of
        JsonStr s -> Just s
        _ -> Nothing

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
    if null input
        then return [pair]
        else if take 1 input == ","
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
    if null input
        then emptyP
        else parseBasedOnFirstChar input

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

-- | Parse a number if possible
parseNumberIfPossible :: String -> Parser JsonVal
parseNumberIfPossible input =
    if not (null input) && isNumberStart (head input)
        then parseJsonNumber
        else emptyP

-- | Check if a character can start a number
isNumberStart :: Char -> Bool
isNumberStart c = isDigit c || c == '-' || c == '.'

-- | Check if a character can be part of a number
isNumberChar :: Char -> Bool
isNumberChar c = isDigit c || c == '.' ||
                 c == '-' || c == '+' || c == 'e' || c == 'E'

-- | Parse a JSON string with proper escaping
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
parseStringHelper acc ('\\':c:rest) = parseStringHelper (c:acc) rest
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
    parseItemRest item

-- | Parse the rest of the items after the first one
parseItemRest :: JsonVal -> Parser [JsonVal]
parseItemRest item = do
    skipWhitespace
    input <- currentInput
    if null input
        then return [item]
        else if take 1 input == ","
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
parseJsonTrue =
    stringP "true" >>
    return (JsonBool True)

-- | Parse JSON false value
parseJsonFalse :: Parser JsonVal
parseJsonFalse =
    stringP "false" >>
    return (JsonBool False)

-- | Parse JSON null value
parseJsonNull :: Parser JsonVal
parseJsonNull =
    stringP "null" >>
    return JsonNull

-- | Parse a JSON number
parseJsonNumber :: Parser JsonVal
parseJsonNumber = Parser $ \input ->
    let (numStr, rest) = span isNumberChar input
    in if null numStr
       then Nothing
       else Just (JsonNum (safeRead numStr), rest)

-- | Safe read function for numbers
safeRead :: String -> Double
safeRead str =
    case reads str of
        [(num, "")] -> num
        _ -> 0.0
