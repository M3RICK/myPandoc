{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- XML Parser implementation with proper style
-}

module XmlParser
    ( parseXml
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
type XmlResult = Maybe (Document, String)

-- | Check if a character can be ignored (whitespace)
isIgnorable::Char -> Bool
isIgnorable c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Skip past the boring whitespace chars
skipBoring::Parser String
skipBoring = manyP (satisfy isIgnorable)

-- | First skip whitespace, then apply parser
withoutBoring::Parser a -> Parser a
withoutBoring p = thenP skipBoring $ \_ input -> run p input

-- | Find an attribute or use default value
findAttrOr::String -> [(String, String)] -> String -> String
findAttrOr name attribs fallback =
    case lookup name attribs of
        Just val -> val
        Nothing -> fallback

-- | Find an optional attribute value
findOptionalAttr::String -> [(String, String)] -> Maybe String
findOptionalAttr = lookup

-- | Create document header from attributes
createHeader::[(String, String)] -> Header
createHeader attribs = Header
    { title = findAttrOr "title" attribs ""
    , author = findOptionalAttr "author" attribs
    , date = findOptionalAttr "date" attribs
    }

-- | Parse first character of XML tag name (letters or underscore)
parseNameStart::Parser Char
parseNameStart = orElse letter (char '_')

-- | Parse subsequent character of XML tag name
parseNameChar::Parser Char
parseNameChar = orElse alphaNum (oneOf "_-:")

-- | Parse a complete XML tag name
parseTagName::Parser String
parseTagName = map2P (:) parseNameStart (manyP parseNameChar)

-- | Check if we're looking at a tag
lookingAtTag::String -> Bool
lookingAtTag input = null input || head input == '<'

-- | Gather text characters until a tag
gatherTextUntilTag::String -> String -> (String, String)
gatherTextUntilTag collected ('<':rest) = (reverse collected, '<':rest)
gatherTextUntilTag collected (c:rest) = gatherTextUntilTag (c:collected) rest
gatherTextUntilTag collected [] = (reverse collected, [])

-- | Parse plain text until a tag starts
collectPlainText::Parser String
collectPlainText = Parser $ \input ->
    if lookingAtTag input then
        Just ("", input)
    else
        Just (gatherTextUntilTag [] input)

-- | Convert text to document paragraph
makeTextNode::Parser Content
makeTextNode = mapP textToContent collectPlainText
  where
    textToContent "" = Paragraph []
    textToContent txt = Paragraph [PlainText txt]

-- | Gather quoted attribute text
gatherQuoted::String -> String -> Maybe (String, String)
gatherQuoted collected ('"':rest) = Just (reverse collected, rest)
gatherQuoted collected (c:rest) = gatherQuoted (c:collected) rest
gatherQuoted _ [] = Nothing

-- | Parse a quoted attribute value
readQuotedValue::Parser String
readQuotedValue = Parser $ \input ->
    case dropWhile isIgnorable input of
        ('"':rest) -> gatherQuoted [] rest
        _ -> Nothing

-- | Parse an attribute name
readAttrName::Parser String
readAttrName = withoutBoring parseTagName

-- | Parse equals sign in attribute
readEquals::Parser ()
readEquals = Parser $ \input ->
    case dropWhile isIgnorable input of
        ('=':rest) -> Just ((), rest)
        _ -> Nothing

-- | Parse value part of attribute after equals
readAttrValue::String -> String -> Maybe ((String, String), String)
readAttrValue name rest2 =
    case run readQuotedValue (dropWhile isIgnorable rest2) of
        Nothing -> Nothing
        Just (value, rest3) -> Just ((name, value), rest3)

-- | Read equals sign and value in attribute
readEqualsAndValue::String -> String -> Maybe ((String, String), String)
readEqualsAndValue name rest1 =
    case run readEquals rest1 of
        Nothing -> Nothing
        Just (_, rest2) -> readAttrValue name rest2

-- | Parse a complete attribute (name="value")
readAttribute::Parser (String, String)
readAttribute = Parser $ \input ->
    case run readAttrName input of
        Nothing -> Nothing
        Just (name, rest1) -> readEqualsAndValue name rest1

-- | Parse multiple attributes in a tag
readAttributes::Parser [(String, String)]
readAttributes = manyP readAttribute

-- | Parse opening document tag
parseDocBegin::Parser ()
parseDocBegin = mapP (const ()) (withoutBoring (stringP "<document>"))

-- | Parse an opening tag with a name
readOpenTag::String -> Parser ()
readOpenTag tagName = mapP (const ())
    (withoutBoring (stringP ("<" ++ tagName)))

-- | Find closing angle bracket after attributes
findClosingAngle::[(String, String)] -> String -> Maybe ([(String, String)], String)
findClosingAngle attrs rest2 =
    case dropWhile isIgnorable rest2 of
        ('>':rest3) -> Just (attrs, rest3)
        _ -> Nothing

-- | Parse attributes and look for closing angle
parseAttrsAndAngle::String -> Maybe ([(String, String)], String)
parseAttrsAndAngle rest1 =
    case run readAttributes rest1 of
        Nothing -> Nothing
        Just (attrs, rest2) -> findClosingAngle attrs rest2

-- | Parse tag opening with attributes
readOpeningTagWithAttrs::String -> Parser [(String, String)]
readOpeningTagWithAttrs tagName = Parser $ \input ->
    case run (readOpenTag tagName) input of
        Nothing -> Nothing
        Just (_, rest1) -> parseAttrsAndAngle rest1

-- | Parse a closing tag
readClosingTag::String -> Parser ()
readClosingTag tagName = withoutBoring $ Parser $ \input ->
    case run (stringP ("</")) (dropWhile isIgnorable input) of
        Nothing -> Nothing
        Just (_, rest1) -> parseClosingTagName tagName rest1

-- | Parse tag name in closing tag
parseClosingTagName::String -> String -> Maybe ((), String)
parseClosingTagName tagName rest1 =
    case run (withoutBoring $ stringP tagName) rest1 of
        Nothing -> Nothing
        Just (_, rest2) -> findEndTagClose rest2

-- | Find closing angle bracket for end tag
findEndTagClose::String -> Maybe ((), String)
findEndTagClose rest2 =
    case dropWhile isIgnorable rest2 of
        ('>':rest3) -> Just ((), rest3)
        _ -> Nothing

-- | Parse the document header section
parseHeaderSection::Parser Header
parseHeaderSection = Parser $ \input ->
    case run (readOpeningTagWithAttrs "header") input of
        Nothing -> Nothing
        Just (attrs, rest1) -> completeHeaderSection attrs rest1

-- | Complete header section after attributes
completeHeaderSection::[(String, String)] -> String -> Maybe (Header, String)
completeHeaderSection attrs rest1 =
    case run (readClosingTag "header") rest1 of
        Nothing -> Nothing
        Just (_, rest2) -> Just (createHeader attrs, rest2)

-- | Parse document body begin tag
parseBodyBegin::Parser ()
parseBodyBegin = withoutBoring $ mapP (const ()) (stringP "<body>")

-- | Parse document body end tag
parseBodyEnd::Parser ()
parseBodyEnd = withoutBoring $ mapP (const ()) (stringP "</body>")

-- | Parse document end tag
parseDocEnd::Parser ()
parseDocEnd = withoutBoring $ mapP (const ()) (stringP "</document>")

-- | Parse paragraph content
parseParagraphContent::String -> Maybe (Content, String)
parseParagraphContent rest1 =
    case run collectPlainText rest1 of
        Nothing -> Nothing
        Just (text, rest2) -> closeParagraph text rest2

-- | Close paragraph and create node
closeParagraph::String -> String -> Maybe (Content, String)
closeParagraph text rest2 =
    case run (withoutBoring $ stringP "</paragraph>") rest2 of
        Nothing -> Nothing
        Just (_, rest3) -> Just (Paragraph [PlainText text], rest3)

-- | Parse a paragraph element
readParagraph::Parser Content
readParagraph = Parser $ \input ->
    case run (withoutBoring $ stringP "<paragraph>") input of
        Nothing -> Nothing
        Just (_, rest1) -> parseParagraphContent rest1

-- | Parse section attributes
parseSectionAttributes::String -> Maybe (Content, String)
parseSectionAttributes rest1 =
    case run readAttributes rest1 of
        Nothing -> Nothing
        Just (attrs, rest2) -> finishSectionOpening attrs rest2

-- | Find closing angle and continue section
finishSectionOpening::[(String, String)] -> String -> Maybe (Content, String)
finishSectionOpening attrs rest2 =
    case dropWhile isIgnorable rest2 of
        ('>':rest3) -> parseSectionBody attrs rest3
        _ -> Nothing

-- | Parse section body content
parseSectionBody::[(String, String)] -> String -> Maybe (Content, String)
parseSectionBody attrs rest3 =
    case run parseContent rest3 of
        Nothing -> Nothing
        Just (children, rest4) -> closeSection attrs children rest4

-- | Close section and create node
closeSection::[(String, String)] -> [Content] -> String -> Maybe (Content, String)
closeSection attrs children rest4 =
    case run (withoutBoring $ stringP "</section>") rest4 of
        Nothing -> Nothing
        Just (_, rest5) -> Just (Section (lookup "title" attrs) children, rest5)

-- | Parse a section element
readSection::Parser Content
readSection = Parser $ \input ->
    case run (withoutBoring $ stringP "<section") input of
        Nothing -> Nothing
        Just (_, rest1) -> parseSectionAttributes rest1

-- | Check for paragraph start
isParagraphStart::String -> Bool
isParagraphStart str = take 11 str == "<paragraph>"

-- | Check for section start
isSectionStart::String -> Bool
isSectionStart str = take 8 str == "<section"

-- | Check for end tag
isEndTag::String -> Bool
isEndTag str = take 2 str == "</"

-- | Identify node type based on string prefix
identifyNodeType::String -> Maybe (Content, String)
identifyNodeType "" = Nothing
identifyNodeType input =
    let trimmed = dropWhile isIgnorable input in
    chooseParserByPrefix trimmed

-- | Choose parser based on string prefix
chooseParserByPrefix::String -> Maybe (Content, String)
chooseParserByPrefix trimmed
    | head trimmed /= '<' = run makeTextNode trimmed
    | isParagraphStart trimmed = run readParagraph trimmed
    | isSectionStart trimmed = run readSection trimmed
    | isEndTag trimmed = Nothing
    | otherwise = run makeTextNode trimmed

-- | Parse any XML node
parseNode::Parser Content
parseNode = withoutBoring $ Parser $ \input ->
    identifyNodeType input

-- | Parse content (sequence of nodes)
parseContent::Parser [Content]
parseContent = manyP parseNode

-- | Parse the document body
parseBodyContent::Parser [Content]
parseBodyContent = thenP parseBodyBegin $ \_ input ->
    case run parseContent input of
        Nothing -> Nothing
        Just (contents, rest) -> finalizeBody contents rest

-- | Finalize body with closing tag
finalizeBody::[Content] -> String -> Maybe ([Content], String)
finalizeBody contents rest =
    case run parseBodyEnd rest of
        Nothing -> Nothing
        Just (_, final) -> Just (contents, final)

-- | Parse XML document after opening tag
parseDocAfterOpen::String -> Maybe (Document, String)
parseDocAfterOpen rest1 =
    case run parseHeaderSection rest1 of
        Nothing -> Nothing
        Just (hdr, afterHeader) -> parseDocBody hdr afterHeader

-- | Parse document body
parseDocBody::Header -> String -> Maybe (Document, String)
parseDocBody hdr afterHeader =
    case run parseBodyContent afterHeader of
        Nothing -> Nothing
        Just (body, afterBody) -> finalizeDoc hdr body afterBody

-- | Finalize document with closing tag
finalizeDoc::Header -> [Content] -> String -> Maybe (Document, String)
finalizeDoc hdr body afterBody =
    case run parseDocEnd afterBody of
        Nothing -> Nothing
        Just (_, afterDocEnd) -> Just (Document hdr body, afterDocEnd)

-- | Parse a complete XML document
parseXml::Parser Document
parseXml = Parser $ \input ->
    let trimmedInput = dropWhile isIgnorable input in
    case run parseDocBegin trimmedInput of
        Nothing -> Nothing
        Just (_, rest1) -> parseDocAfterOpen rest1
