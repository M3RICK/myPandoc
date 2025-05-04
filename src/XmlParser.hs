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
    _ <- skipWhitespace
    header' <- parseEPITECHHeader
    _ <- skipWhitespace
    content' <- parseDocumentBody
    _ <- skipWhitespace
    _ <- stringP "</document>"
    _ <- skipWhitespace
    return (Document header' content')

parseEPITECHHeader::Parser Header
parseEPITECHHeader = do
    _ <- skipWhitespace
    _ <- stringP "<header"
    _ <- skipWhitespace
    attrs <- parseAttributes
    _ <- skipWhitespace
    _ <- stringP ">"
    _ <- skipWhitespace
    _ <- stringP "</header>"
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
    _ <- stringP "<body>"
    _ <- skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
    _ <- stringP "</body>"
    return contents

parseAttributes::Parser [(String, String)]
parseAttributes = manyP parseAttribute

parseAttribute::Parser (String, String)
parseAttribute = do
    _ <- skipWhitespace
    name <- parseAttributeName
    _ <- skipWhitespace
    _ <- char '='
    _ <- skipWhitespace
    _ <- char '"'
    value <- manyP (satisfy (/= '"'))
    _ <- char '"'
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
    _ <- skipWhitespace
    _ <- stringP "<paragraph>"
    inlines <- parseInlines
    _ <- stringP "</paragraph>"
    return (Paragraph inlines)

parseSection::Parser Content
parseSection = do
    sectionAttrs <- parseSectionTag
    _ <- skipWhitespace
    contents <- parseSectionContents
    return (Section (lookup "title" sectionAttrs) contents)

parseSectionTag::Parser [(String, String)]
parseSectionTag = do
    _ <- skipWhitespace
    _ <- stringP "<section"
    _ <- skipWhitespace
    attrs <- parseAttributes
    _ <- skipWhitespace
    _ <- char '>'
    return attrs

parseSectionContents::Parser [Content]
parseSectionContents = do
    contents <- manyP (parseContent <* skipWhitespace)
    _ <- stringP "</section>"
    return contents

parseCodeBlock::Parser Content
parseCodeBlock = do
    _ <- skipWhitespace
    _ <- stringP "<codeblock>"
    code <- manyP (satisfy (/= '<'))
    _ <- stringP "</codeblock>"
    return (CodeBlock code)

parseList::Parser Content
parseList = do
    _ <- skipWhitespace
    _ <- stringP "<list>"
    _ <- skipWhitespace
    items <- manyP (parseListItem <* skipWhitespace)
    _ <- stringP "</list>"
    return (List items)

parseListItem::Parser ListItem
parseListItem = do
    _ <- skipWhitespace
    _ <- stringP "<item>"
    inlines <- parseInlines
    _ <- stringP "</item>"
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
    _ <- stringP "<bold>"
    inlines <- parseInlines
    _ <- stringP "</bold>"
    return (Bold inlines)

parseItalic::Parser Inline
parseItalic = do
    _ <- stringP "<italic>"
    inlines <- parseInlines
    _ <- stringP "</italic>"
    return (Italic inlines)

parseCode::Parser Inline
parseCode = do
    _ <- stringP "<code>"
    text <- manyP (satisfy (/= '<'))
    _ <- stringP "</code>"
    return (Code text)

parseLink::Parser Inline
parseLink = do
    _ <- stringP "<link"
    _ <- skipWhitespace
    attrs <- parseAttributes
    _ <- skipWhitespace
    _ <- char '>'
    text <- manyP (satisfy (/= '<'))
    _ <- stringP "</link>"
    let url = findAttr "href" attrs ""
    return (Link text url)

parseImage::Parser Inline
parseImage = do
    _ <- stringP "<image"
    _ <- skipWhitespace
    attrs <- parseAttributes
    _ <- skipWhitespace
    _ <- stringP ">"
    _ <- skipWhitespace
    _ <- stringP "</image>"
    let url = findAttr "src" attrs ""
    let alt = findAttr "alt" attrs ""
    return (Image alt url)