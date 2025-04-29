{-
-- EPITECH PROJECT, 2024
-- XmlParser.hs
-- File description:
-- XML Parser implementation for document conversion
-}

module XmlParser
    ( parseXml
    ) where

import ParsingLibrary
import Document

-- | Parse a complete XML document
parseXml :: Parser Document
parseXml = 
    skipWhitespace *>
    stringP "<document>" *>
    skipWhitespace *>
    parseHeader >>= \header' ->
    skipWhitespace *>
    stringP "<body>" *>
    skipWhitespace *>
    parseContents >>= \content' ->
    skipWhitespace *>
    stringP "</body>" *>
    skipWhitespace *>
    stringP "</document>" *>
    skipWhitespace *>
    return (Document header' content')

-- | Parse XML header section
parseHeader :: Parser Header
parseHeader = 
    stringP "<header" *>
    skipWhitespace *>
    parseAttributes >>= \attrs ->
    skipWhitespace *>
    stringP ">" *>
    skipWhitespace *>
    stringP "</header>" *>
    let title' = findAttr "title" attrs ""
        author' = lookup "author" attrs
        date' = lookup "date" attrs
    in return (Header title' author' date')

-- | Parse attributes in an XML tag
parseAttributes :: Parser [(String, String)]
parseAttributes = manyP parseAttribute

-- | Parse a single attribute (name="value")
parseAttribute :: Parser (String, String)
parseAttribute = 
    skipWhitespace *>
    parseAttributeName >>= \name ->
    skipWhitespace *>
    char '=' *>
    skipWhitespace *>
    char '"' *>
    parseAttributeValue >>= \value ->
    char '"' *>
    return (name, value)

-- | Parse attribute name
parseAttributeName :: Parser String
parseAttributeName = someP (satisfy isAttributeNameChar)
  where
    isAttributeNameChar c = (c >= 'a' && c <= 'z') || 
                          (c >= 'A' && c <= 'Z') || 
                          (c >= '0' && c <= '9') || 
                          c == '_' || c == '-'

-- | Parse attribute value (characters between quotes)
parseAttributeValue :: Parser String
parseAttributeValue = manyP (satisfy (/= '"'))

-- | Get attribute value with default
findAttr :: String -> [(String, String)] -> String -> String
findAttr name attrs defaultValue = 
    case lookup name attrs of
        Just value -> value
        Nothing -> defaultValue

-- | Parse document contents
parseContents :: Parser [Content]
parseContents = manyP parseContent

-- | Parse a content element
parseContent :: Parser Content
parseContent = parseParagraph `orElse` parseSection `orElse` parseCodeBlock `orElse` parseList

-- | Parse a paragraph element
parseParagraph :: Parser Content
parseParagraph = 
    skipWhitespace *>
    stringP "<paragraph>" *>
    parseInlines >>= \inlines ->
    stringP "</paragraph>" *>
    return (Paragraph inlines)

-- | Parse a section element
parseSection :: Parser Content
parseSection = 
    skipWhitespace *>
    stringP "<section" *>
    skipWhitespace *>
    parseAttributes >>= \attrs ->
    skipWhitespace *>
    stringP ">" *>
    skipWhitespace *>
    parseContents >>= \contents ->
    skipWhitespace *>
    stringP "</section>" *>
    return (Section (lookup "title" attrs) contents)

-- | Parse a code block element
parseCodeBlock :: Parser Content
parseCodeBlock = 
    skipWhitespace *>
    stringP "<codeblock>" *>
    parseCodeContent >>= \code ->
    stringP "</codeblock>" *>
    return (CodeBlock code)

-- | Parse code content (all text until closing tag)
parseCodeContent :: Parser String
parseCodeContent = Parser $ \input ->
    let (code, rest) = span (/= '<') input
    in if take 12 rest == "</codeblock>"
       then Just (code, rest)
       else Nothing

-- | Parse a list element
parseList :: Parser Content
parseList = 
    skipWhitespace *>
    stringP "<list>" *>
    skipWhitespace *>
    manyP parseListItem >>= \items ->
    skipWhitespace *>
    stringP "</list>" *>
    return (List items)

-- | Parse a list item
parseListItem :: Parser ListItem
parseListItem = 
    skipWhitespace *>
    stringP "<item>" *>
    parseInlines >>= \inlines ->
    stringP "</item>" *>
    return (ListItem inlines)

-- | Parse inline elements
parseInlines :: Parser [Inline]
parseInlines = manyP parseInline

-- | Parse a single inline element
parseInline :: Parser Inline
parseInline = parsePlainText `orElse` parseBold `orElse` parseItalic `orElse` 
            parseCode `orElse` parseLink `orElse` parseImage

-- | Parse plain text
parsePlainText :: Parser Inline
parsePlainText = Parser $ \input ->
    case input of
        [] -> Nothing
        ('<':_) -> Nothing
        _ -> let (text, rest) = span (/= '<') input
             in if null text then Nothing else Just (PlainText text, rest)

-- | Parse bold text
parseBold :: Parser Inline
parseBold = 
    stringP "<bold>" *>
    parseInlines >>= \inlines ->
    stringP "</bold>" *>
    return (Bold inlines)

-- | Parse italic text
parseItalic :: Parser Inline
parseItalic = 
    stringP "<italic>" *>
    parseInlines >>= \inlines ->
    stringP "</italic>" *>
    return (Italic inlines)

-- | Parse code inline
parseCode :: Parser Inline
parseCode = 
    stringP "<code>" *>
    manyP (satisfy (/= '<')) >>= \code ->
    stringP "</code>" *>
    return (Code code)

-- | Parse link
parseLink :: Parser Inline
parseLink = 
    stringP "<link" *>
    skipWhitespace *>
    parseAttributes >>= \attrs ->
    skipWhitespace *>
    stringP ">" *>
    manyP (satisfy (/= '<')) >>= \text ->
    stringP "</link>" *>
    let url = findAttr "href" attrs ""
    in return (Link text url)

-- | Parse image
parseImage :: Parser Inline
parseImage = 
    stringP "<image" *>
    skipWhitespace *>
    parseAttributes >>= \attrs ->
    skipWhitespace *>
    stringP "></image>" *>
    let alt = findAttr "alt" attrs ""
        src = findAttr "src" attrs ""
    in return (Image alt src)