{-
-- EPITECH PROJECT, 2024
-- XmlParser.hs
-- File description:
-- XML Parser
-}

module XmlParser
    ( parseXml
    ) where

import ParsingLibrary
import Document
import Data.Char (isAlphaNum)

parseXml::Parser Document
parseXml =
    skipWhitespace >>
    stringP "<document>" >>
    parseDocumentContent

parseDocumentContent::Parser Document
parseDocumentContent = do
    skipWhitespace
    header' <- parseEPITECHHeader
    skipWhitespace
    content' <- parseDocumentBody
    skipWhitespace
    stringP "</document>"
    skipWhitespace
    return (Document header' content')

parseEPITECHHeader::Parser Header
parseEPITECHHeader = do
    skipWhitespace
    stringP "<header"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    stringP ">"
    skipWhitespace
    stringP "</header>"
    let title' = findAttr "title" attrs ""
    let author' = findAttrOpt "author" attrs
    let date' = findAttrOpt "date" attrs
    return (Header title' author' date')

findAttrOpt::String -> [(String, String)] -> Maybe String
findAttrOpt name attrs =
    case lookup name attrs of
        Just value -> Just value
        Nothing -> Nothing

parseDocumentBody::Parser [Content]
parseDocumentBody = do
    stringP "<body>"
    skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
    stringP "</body>"
    return contents

parseAttributes::Parser [(String, String)]
parseAttributes = manyP parseAttribute

parseAttribute::Parser (String, String)
parseAttribute = do
    skipWhitespace
    name <- parseAttributeName
    skipWhitespace
    char '='
    skipWhitespace
    char '"'
    value <- manyP (satisfy (/= '"'))
    char '"'
    return (name, value)

isAttributeNameChar::Char -> Bool
isAttributeNameChar c = isAlphaNum c || c == '_' || c == '-'

parseAttributeName::Parser String
parseAttributeName = someP (satisfy isAttributeNameChar)

findAttr::String -> [(String, String)] -> String -> String
findAttr name attrs defaultValue =
    case lookup name attrs of
        Just value -> value
        Nothing -> defaultValue

parseContent::Parser Content
parseContent = 
    parseSection `orElse`
    parseParagraph `orElse`
    parseCodeBlock `orElse`
    parseList

parseParagraph::Parser Content
parseParagraph = do
    skipWhitespace
    stringP "<paragraph>"
    inlines <- parseInlines
    stringP "</paragraph>"
    return (Paragraph inlines)

parseSection::Parser Content
parseSection = do
    sectionAttrs <- parseSectionTag
    skipWhitespace
    contents <- parseSectionContents
    return (Section (lookup "title" sectionAttrs) contents)

parseSectionTag::Parser [(String, String)]
parseSectionTag = do
    skipWhitespace
    stringP "<section"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    return attrs

parseSectionContents::Parser [Content]
parseSectionContents = do
    contents <- manyP (parseContent <* skipWhitespace)
    stringP "</section>"
    return contents

parseCodeBlock::Parser Content
parseCodeBlock = do
    skipWhitespace
    stringP "<codeblock>"
    code <- manyP (satisfy (/= '<'))
    stringP "</codeblock>"
    return (CodeBlock code)

parseList::Parser Content
parseList = do
    skipWhitespace
    stringP "<list>"
    skipWhitespace
    items <- manyP (parseListItem <* skipWhitespace)
    stringP "</list>"
    return (List items)

parseListItem::Parser ListItem
parseListItem = do
    skipWhitespace
    stringP "<item>"
    inlines <- parseInlines
    stringP "</item>"
    return (ListItem inlines)

parseInlines::Parser [Inline]
parseInlines = manyP parseInline

parseInline::Parser Inline
parseInline = parseFormattedInline `orElse` parsePlainText

parseFormattedInline::Parser Inline
parseFormattedInline =
    parseBold `orElse`
    parseItalic `orElse`
    parseCode `orElse`
    parseLink `orElse`
    parseImage

parsePlainText::Parser Inline
parsePlainText = Parser $ \input ->
    let (text, rest) = span (/= '<') input
    in if null text then Nothing else Just (PlainText text, rest)

parseBold::Parser Inline
parseBold = do
    stringP "<bold>"
    inlines <- parseInlines
    stringP "</bold>"
    return (Bold inlines)

parseItalic::Parser Inline
parseItalic = do
    stringP "<italic>"
    inlines <- parseInlines
    stringP "</italic>"
    return (Italic inlines)

parseCode::Parser Inline
parseCode = do
    stringP "<code>"
    text <- manyP (satisfy (/= '<'))
    stringP "</code>"
    return (Code text)

parseLink::Parser Inline
parseLink = do
    stringP "<link"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    text <- manyP (satisfy (/= '<'))
    stringP "</link>"
    let url = findAttr "href" attrs ""
    return (Link text url)

parseImage::Parser Inline
parseImage = do
    stringP "<image"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    stringP ">"
    skipWhitespace
    stringP "</image>"
    let url = findAttr "src" attrs ""
    let alt = findAttr "alt" attrs ""
    return (Image alt url)