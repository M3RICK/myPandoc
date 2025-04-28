{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- XML Parser implementation for document conversion
-}

module XmlParser
    ( parseXml
    , XmlParseResult
    , parseDocBegin
    , parseHeaderSection
    , parseBodyContent
    , parseDocEnd
    ) where

import ParsingLibrary
import Document
import Data.Char (isSpace, toLower)
import Data.List (intercalate)

-- | The result type from parsing XML
type XmlParseResult = Maybe (Document, String)

-- | Check if a character is XML whitespace
isXmlSpace::Char -> Bool
isXmlSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Skip whitespace characters
skipWhite::Parser String
skipWhite = manyP (satisfy isXmlSpace)

-- | Apply a parser after skipping whitespace
applyAfterWhite::Parser a -> Parser a
applyAfterWhite p = thenP skipWhite $ \_ input -> run p input

-- | Extract attribute with default value
getAttrOrDefault::String -> [(String, String)] -> String -> String
getAttrOrDefault name attribs fallback =
    case lookup name attribs of
        Just val -> val
        Nothing -> fallback

-- | Extract optional attribute value
getOptionalAttr::String -> [(String, String)] -> Maybe String
getOptionalAttr = lookup

-- | Create a header from attributes
buildHeader::[(String, String)] -> Header
buildHeader attribs = Header
    { title = getAttrOrDefault "title" attribs ""
    , author = getOptionalAttr "author" attribs
    , date = getOptionalAttr "date" attribs
    }

-- | Parse first character of an XML name
parseFirstChar::Parser Char
parseFirstChar = orElse letter (char '_')

-- | Parse subsequent character of an XML name
parseRestChar::Parser Char
parseRestChar = orElse alphaNum (oneOf "_-:")

-- | Parse a complete XML name/identifier
parseXmlName::Parser String
parseXmlName = map2P (:) parseFirstChar (manyP parseRestChar)

-- | Check if input is empty or starts with a tag
isTagStart::String -> Bool
isTagStart input = null input || head input == '<'

-- | Collect text characters until a tag
collectText::String -> String -> (String, String)
collectText acc ('<':rest) = (reverse acc, '<':rest)
collectText acc (c:rest) = collectText (c:acc) rest
collectText acc [] = (reverse acc, [])

-- | Parse text until a tag starts
parseText::Parser String
parseText = Parser $ \input ->
    if isTagStart input then
        Just ("", input)
    else
        Just (collectText [] input)

-- | Convert raw text to document content
createTextNode::Parser Content
createTextNode = mapP toContent parseText
  where
    toContent "" = Paragraph []
    toContent txt = Paragraph [PlainText txt]

-- | Collect quoted text until end quote
collectQuoted::String -> String -> Maybe (String, String)
collectQuoted acc ('"':rest) = Just (reverse acc, rest)
collectQuoted acc (c:rest) = collectQuoted (c:acc) rest
collectQuoted _ [] = Nothing

-- | Parse a quoted attribute value
parseQuotedValue::Parser String
parseQuotedValue = Parser $ \input ->
    case dropWhile isXmlSpace input of
        ('"':rest) -> collectQuoted [] rest
        _ -> Nothing

-- | Parse an attribute name
parseAttrName::Parser String
parseAttrName = applyAfterWhite parseXmlName

-- | Parse the equals sign for attributes
parseEquals::Parser ()
parseEquals = Parser $ \input ->
    case dropWhile isXmlSpace input of
        ('=':rest) -> Just ((), rest)
        _ -> Nothing

-- | Parse attribute value after finding equals sign
parseAttrValue::String -> String -> Maybe ((String, String), String)
parseAttrValue name rest2 =
    case run parseQuotedValue (dropWhile isXmlSpace rest2) of
        Nothing -> Nothing
        Just (value, rest3) -> Just ((name, value), rest3)

-- | Handle equals sign in attribute parsing
handleAttrEquals::String -> String -> Maybe ((String, String), String)
handleAttrEquals name rest1 =
    case run parseEquals rest1 of
        Nothing -> Nothing
        Just (_, rest2) -> parseAttrValue name rest2

-- | Parse a complete attribute (name="value")
parseAttribute::Parser (String, String)
parseAttribute = Parser $ \input ->
    case run parseAttrName input of
        Nothing -> Nothing
        Just (name, rest1) -> handleAttrEquals name rest1

-- | Parse multiple attributes
parseAttributes::Parser [(String, String)]
parseAttributes = manyP parseAttribute

-- | Parse document begin tag
parseDocBegin::Parser ()
parseDocBegin = mapP (const ()) (applyAfterWhite (stringP "<document>"))

-- | Parse a tag opener with a name
parseTagOpener::String -> Parser ()
parseTagOpener tagName = mapP (const ())
    (applyAfterWhite (stringP ("<" ++ tagName)))

-- | Find closing angle bracket after attributes
findClosingAngle::[(String, String)] -> String -> Maybe ([(String, String)], String)
findClosingAngle attrs rest2 =
    case dropWhile isXmlSpace rest2 of
        ('>':rest3) -> Just (attrs, rest3)
        _ -> Nothing

-- | Parse attributes and find closing angle
parseAttrsAndClose::String -> Maybe ([(String, String)], String)
parseAttrsAndClose rest1 =
    case run parseAttributes rest1 of
        Nothing -> Nothing
        Just (attrs, rest2) -> findClosingAngle attrs rest2

-- | Parse a tag opener with attributes
parseOpeningTag::String -> Parser [(String, String)]
parseOpeningTag tagName = Parser $ \input ->
    case run (parseTagOpener tagName) input of
        Nothing -> Nothing
        Just (_, rest1) -> parseAttrsAndClose rest1

-- | Find closing angle bracket for end tag
findEndTagClose::String -> Maybe ((), String)
findEndTagClose rest2 =
    case dropWhile isXmlSpace rest2 of
        ('>':rest3) -> Just ((), rest3)
        _ -> Nothing

-- | Parse tag name in closing tag
parseEndTagName::String -> String -> Maybe ((), String)
parseEndTagName tagName rest1 =
    case run (applyAfterWhite $ stringP tagName) rest1 of
        Nothing -> Nothing
        Just (_, rest2) -> findEndTagClose rest2

-- | Parse a closing tag prefix
parseClosePrefix::String -> Maybe ((), String)
parseClosePrefix input =
    case run (stringP ("</")) (dropWhile isXmlSpace input) of
        Nothing -> Nothing
        Just (_, rest1) -> Just ((), rest1)

-- | Parse a closing tag
parseClosingTag::String -> Parser ()
parseClosingTag tagName = applyAfterWhite $ Parser $ \input ->
    case parseClosePrefix input of
        Nothing -> Nothing
        Just (_, rest1) -> parseEndTagName tagName rest1

-- | Parse header closing tag
parseHeaderClose::[(String, String)] -> String -> Maybe (Header, String)
parseHeaderClose attrs rest1 =
    case run (parseClosingTag "header") rest1 of
        Nothing -> Nothing
        Just (_, rest2) -> Just (buildHeader attrs, rest2)

-- | Parse the header section
parseHeaderSection::Parser Header
parseHeaderSection = Parser $ \input ->
    case run (parseOpeningTag "header") input of
        Nothing -> Nothing
        Just (attrs, rest1) -> parseHeaderClose attrs rest1

-- | Parse document body begin tag
parseBodyBegin::Parser ()
parseBodyBegin = applyAfterWhite $ mapP (const ()) (stringP "<body>")

-- | Parse document body end tag
parseBodyEnd::Parser ()
parseBodyEnd = applyAfterWhite $ mapP (const ()) (stringP "</body>")

-- | Parse document end tag
parseDocEnd::Parser ()
parseDocEnd = applyAfterWhite $ mapP (const ()) (stringP "</document>")

-- | Parse paragraph content
parseParagraphContent::String -> Maybe (Content, String)
parseParagraphContent rest1 =
    case run parseText rest1 of
        Nothing -> Nothing
        Just (text, rest2) -> closeParagraph text rest2

-- | Handle paragraph closing tag
closeParagraph::String -> String -> Maybe (Content, String)
closeParagraph text rest2 =
    case run (applyAfterWhite $ stringP "</paragraph>") rest2 of
        Nothing -> Nothing
        Just (_, rest3) -> Just (Paragraph [PlainText text], rest3)

-- | Parse a paragraph element
parseParagraph::Parser Content
parseParagraph = Parser $ \input ->
    case run (applyAfterWhite $ stringP "<paragraph>") input of
        Nothing -> Nothing
        Just (_, rest1) -> parseParagraphContent rest1

-- | Parse section attributes
parseSectionAttrs::String -> Maybe (Content, String)
parseSectionAttrs rest1 =
    case run parseAttributes rest1 of
        Nothing -> Nothing
        Just (attrs, rest2) -> findSectionClose attrs rest2

-- | Find section closing angle bracket
findSectionClose::[(String, String)] -> String -> Maybe (Content, String)
findSectionClose attrs rest2 =
    case dropWhile isXmlSpace rest2 of
        ('>':rest3) -> parseSectionContent attrs rest3
        _ -> Nothing

-- | Parse section content
parseSectionContent::[(String, String)] -> String -> Maybe (Content, String)
parseSectionContent attrs rest3 =
    case run parseContent rest3 of
        Nothing -> Nothing
        Just (children, rest4) -> closeSection attrs children rest4

-- | Handle section closing tag
closeSection::[(String, String)] -> [Content] -> String -> Maybe (Content, String)
closeSection attrs children rest4 =
    case run (applyAfterWhite $ stringP "</section>") rest4 of
        Nothing -> Nothing
        Just (_, rest5) -> Just (Section (lookup "title" attrs) children, rest5)

-- | Parse a section element
parseSection::Parser Content
parseSection = Parser $ \input ->
    case run (applyAfterWhite $ stringP "<section") input of
        Nothing -> Nothing
        Just (_, rest1) -> parseSectionAttrs rest1

-- | Check if the input starts with specific tag prefix
startsWithTag::String -> String -> Int -> Bool
startsWithTag input prefix maxLen =
    let prefixLen = length prefix
        inputPrefix = take prefixLen (take maxLen input)
    in inputPrefix == prefix

-- | Check tag type for a non-empty input
checkSpecificTagType::String -> Maybe (Content, String)
checkSpecificTagType trimmed
    | startsWithTag trimmed "<paragraph>" 11 = run parseParagraph trimmed
    | startsWithTag trimmed "<section" 8 = run parseSection trimmed
    | startsWithTag trimmed "</" 2 = Nothing
    | otherwise = run createTextNode trimmed

-- | Determine node type and parse accordingly
decideNodeType::String -> Maybe (Content, String)
decideNodeType input
    | null input = Nothing
    | head (dropWhile isXmlSpace input) /= '<' = run createTextNode input
    | otherwise = checkSpecificTagType (dropWhile isXmlSpace input)

-- | Parse any XML node
parseNode::Parser Content
parseNode = applyAfterWhite $ Parser $ \input ->
    decideNodeType input

-- | Parse content (sequence of nodes)
parseContent::Parser [Content]
parseContent = manyP parseNode

-- | Finalize body parsing with end tag
finalizeBody::[Content] -> String -> Maybe ([Content], String)
finalizeBody contents rest =
    case run parseBodyEnd rest of
        Nothing -> Nothing
        Just (_, final) -> Just (contents, final)

-- | Parse the body content
parseBodyContent::Parser [Content]
parseBodyContent = thenP parseBodyBegin $ \_ input ->
    case run parseContent input of
        Nothing -> Nothing
        Just (contents, rest) -> finalizeBody contents rest

-- | Parse document structure
parseDocStructure::String -> Maybe (Document, String)
parseDocStructure input =
    case run parseHeaderSection input of
        Nothing -> Nothing
        Just (hdr, afterHeader) -> parseDocBody hdr afterHeader

-- | Parse document body after header
parseDocBody::Header -> String -> Maybe (Document, String)
parseDocBody hdr input =
    case run parseBodyContent input of
        Nothing -> Nothing
        Just (body, afterBody) -> finalizeDoc hdr body afterBody

-- | Finalize document parsing with closing tag
finalizeDoc::Header -> [Content] -> String -> Maybe (Document, String)
finalizeDoc hdr body input =
    case run parseDocEnd input of
        Nothing -> Nothing
        Just (_, afterDocEnd) -> Just (Document hdr body, afterDocEnd)

-- | Parse a complete XML document
parseXml::Parser Document
parseXml = Parser $ \input ->
    let trimmedInput = dropWhile isXmlSpace input in
    case run parseDocBegin trimmedInput of
        Nothing -> Nothing
        Just (_, rest1) -> parseDocStructure rest1
