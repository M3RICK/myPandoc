{-
-- EPITECH PROJECT, 2025
-- xmlParser.hs
-- File description:
-- Its parsin time
-}

module CustomXmlParser
    ( parseXml
    , XmlParseResult
    ) where

import ParsingLibrary
import Document
import Data.Char (isSpace, toLower)
import Data.List (intercalate)

-- | The result type from parsing XML
type XmlParseResult = Maybe (Document, String)

-- ===========================================
-- CHARACTER HANDLING                        =
-- ===========================================

isWhitespace::Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

skipWhite::Parser String
skipWhite = manyP (satisfy isWhitespace)

gatherUntil::Char -> String -> Maybe (String, String)
gatherUntil stopChar = gather []
  where
    gather result (c:rest) | c == stopChar = Just (reverse result, rest)
    gather result (c:rest) = gather (c:result) rest
    gather _ [] = Nothing

findChar::Char -> String -> Maybe ((), String)
findChar expected str =
    let trimmed = dropWhile isWhitespace str
    in if null trimmed then Nothing
       else if head trimmed == expected
            then Just ((), tail trimmed)
            else Nothing


-- ===========================================
-- ATTRIBUTE HELPERS                         =
-- ===========================================

getAttrOrDefault::String -> [(String, String)] -> String -> String
getAttrOrDefault name attribs fallback =
    case lookup name attribs of
        Just val -> val
        Nothing -> fallback

getOptionalAttr::String -> [(String, String)] -> Maybe String
getOptionalAttr = lookup

makeHeader::[(String, String)] -> Header
makeHeader attribs = Header
    { title = getAttrOrDefault "title" attribs ""
    , author = getOptionalAttr "author" attribs
    , date = getOptionalAttr "date" attribs
    }

-- ===========================================
-- NAME AND IDENTIFIER                       =
-- ===========================================

-- | Parse first character of an XML name
parseFirstChar::Parser Char
parseFirstChar = orElse letter (char '_')

-- | Parse subsequent character of an XML name
parseRestChar::Parser Char
parseRestChar = orElse alphaNum (oneOf "_-:")

-- | Parse a complete XML name/identifier
parseXmlName::Parser String
parseXmlName = map2P (:) parseFirstChar (manyP parseRestChar)

-- | Parse an XML name after skipping whitespace
parseXmlNameAfterSpace::String -> Maybe (String, String)
parseXmlNameAfterSpace input = run parseXmlName (dropWhile isWhitespace input)

-- ===========================================
-- TEXT CONTENT                              =
-- ===========================================

-- | Parse plain text until a tag starts
parseContent::Parser String
parseContent = thenP currentInput $ \input _ ->
    extractText input
  where
    extractText "" = Just ("", "")
    extractText input@('<':_) = Just ("", input)
    extractText input = extractRest [] input

    extractRest acc ('<':rest) = Just (reverse acc, '<':rest)
    extractRest acc (c:rest) = extractRest (c:acc) rest
    extractRest acc [] = Just (reverse acc, [])

-- | Convert raw text to document content
makeTextNode::Parser Content
makeTextNode = mapP textToContent parseContent
  where
    textToContent "" = Paragraph []
    textToContent txt = Paragraph [PlainText txt]

-- ===========================================
-- ATTRIBUTE                                 =
-- ===========================================

-- | Parse a quoted attribute value
parseQuotedValue::Parser String
parseQuotedValue = thenP (char '"') $ \_ input ->
    gatherUntil '"' input

-- | Parse an attribute name
parseAttrName::Parser String
parseAttrName = thenP skipWhite $ \_ input ->
    run parseXmlName input

-- | Match the equals sign for attributes
matchEquals::Parser ()
matchEquals = thenP skipWhite $ \_ input ->
    case input of
        ('=':rest) -> Just ((), rest)
        _ -> Nothing

-- | Parse a complete attribute (name="value")
parseAttr::Parser (String, String)
parseAttr = thenP parseAttrName $ \name input ->
    case run matchEquals input of
        Nothing -> Nothing
        Just (_, afterEq) ->
            case run parseQuotedValue (dropWhile isWhitespace afterEq) of
                Nothing -> Nothing
                Just (value, remaining) -> Just ((name, value), remaining)

-- | Parse multiple attributes
parseAttrs::Parser [(String, String)]
parseAttrs = manyP parseAttr

-- ===========================================
-- TAG                                       =
-- ===========================================

openAngle::Parser Char
openAngle = char '<'

closeAngle::Parser Char
closeAngle = char '>'

parseTagBegin::Parser String
parseTagBegin = thenP openAngle $ \_ input ->
    run parseXmlName (dropWhile isWhitespace input)

parseStartTag::Parser (String, [(String, String)])
parseStartTag = thenP parseTagBegin $ \tagName input ->
    case run parseAttrs input of
        Nothing -> Nothing
        Just (attributes, afterAttrs) ->
            case dropWhile isWhitespace afterAttrs of
                ('>':rest) -> Just ((tagName, attributes), rest)
                _ -> Nothing

parseEndPrefix::Parser ()
parseEndPrefix = thenP (stringP "</") $ \_ _ -> Just ((), "")

parseEndTag::Parser String
parseEndTag = thenP parseEndPrefix $ \_ input ->
    case run parseXmlName (dropWhile isWhitespace input) of
        Nothing -> Nothing
        Just (name, afterName) ->
            case dropWhile isWhitespace afterName of
                ('>':rest) -> Just (name, rest)
                _ -> Nothing

-- ===========================================
-- ELEMENT                                   =
-- ===========================================

decideNodeType::String -> Maybe (Content, String)
decideNodeType input
    | null input = Nothing
    | head input == '<' && take 2 input /= "</" = run parseXmlElement input
    | otherwise = run makeTextNode input

parseNode::Parser Content
parseNode = thenP skipWhite $ \_ input ->
    decideNodeType input

parseElementContent::String -> Parser [Content]
parseElementContent tagName = thenP currentInput $ \input _ ->
    accumulateContent tagName [] input

accumulateContent::String -> [Content] -> String -> Maybe ([Content], String)
accumulateContent tagName nodes input =
    case run parseEndTag input of
        Just (endName, afterEnd) | map toLower endName == map toLower tagName ->
            Just (reverse nodes, afterEnd)
        _ -> case run parseNode input of
            Nothing -> Nothing
            Just (node, afterNode) ->
                accumulateContent tagName (node:nodes) afterNode

parseXmlElement::Parser Content
parseXmlElement = thenP parseStartTag $ \(name, _) afterOpen ->
    case run (parseElementContent name) afterOpen of
        Nothing -> Nothing
        Just (children, finalRest) ->
            Just (Section (Just name) children, finalRest)

parseNodeList::Parser [Content]
parseNodeList = thenP skipWhite $ \_ input ->
    buildNodeList input

buildNodeList::String -> Maybe ([Content], String)
buildNodeList input = case run parseNode input of
    Nothing -> Just ([], input)
    Just (node, rest) -> case run parseNodeList rest of
        Nothing -> Just ([node], rest)
        Just (nodes, finalRest) -> Just (node:nodes, finalRest)

-- ===========================================
-- DOCUMENT STRUCTURE                        =
-- ===========================================

parseDocHeader::Parser [(String, String)]
parseDocHeader = thenP (stringP "<header") $ \_ afterTag ->
    case run parseAttrs afterTag of
        Nothing -> Nothing
        Just (attrs, afterAttrs) ->
            case dropWhile isWhitespace afterAttrs of
                ('>':afterClose) -> Just (attrs, afterClose)
                _ -> Nothing

parseHeaderSection::Parser Header
parseHeaderSection = thenP parseDocHeader $ \attrs afterOpen ->
    case run parseEndTag afterOpen of
        Just (closeName, afterHeader) | map toLower closeName == "header" ->
            Just (makeHeader attrs, afterHeader)
        _ -> Nothing

parseBodyBegin::Parser ()
parseBodyBegin = thenP (stringP "<body>") $ \_ _ -> Just ((), "")

parseBodyEnd::Parser ()
parseBodyEnd = thenP (stringP "</body>") $ \_ _ -> Just ((), "")

parseBodyContent::Parser [Content]
parseBodyContent = thenP parseBodyBegin $ \_ afterOpen ->
    case run parseNodeList afterOpen of
        Nothing -> Nothing
        Just (contents, afterContents) ->
            case run parseBodyEnd afterContents of
                Nothing -> Nothing
                Just (_, afterClose) -> Just (contents, afterClose)

parseDocBegin::Parser ()
parseDocBegin = thenP (stringP "<document>") $ \_ _ -> Just ((), "")

parseDocEnd::Parser ()
parseDocEnd = thenP (stringP "</document>") $ \_ _ -> Just ((), "")

parseXml::Parser Document
parseXml = thenP skipWhite $ \_ input ->
    case run parseDocBegin input of
        Nothing -> Nothing
        Just (_, afterDocOpen) ->
            parseDocStructure afterDocOpen

parseDocStructure::String -> Maybe (Document, String)
parseDocStructure input = case run parseHeaderSection input of
    Nothing -> Nothing
    Just (hdr, afterHeader) ->
        parseDocBody hdr afterHeader

parseDocBody::Header -> String -> Maybe (Document, String)
parseDocBody hdr input = case run parseBodyContent input of
    Nothing -> Nothing
    Just (body, afterBody) ->
        finalizeDoc hdr body afterBody

finalizeDoc::Header -> [Content] -> String -> Maybe (Document, String)
finalizeDoc hdr body input = case run parseDocEnd input of
    Nothing -> Nothing
    Just (_, afterDocEnd) -> Just (Document hdr body, afterDocEnd)
