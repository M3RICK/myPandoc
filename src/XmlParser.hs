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
import Data.Char (isSpace)

-- | Parse a complete XML document
parseXml :: Parser Document
parseXml = do
    skipWhitespace
    _ <- stringP "<document>"
    skipWhitespace
    header <- parseHeader
    skipWhitespace
    _ <- stringP "<body>"
    skipWhitespace
    content <- parseContents
    skipWhitespace
    _ <- stringP "</body>"
    skipWhitespace
    _ <- stringP "</document>"
    skipWhitespace
    return (Document header content)

-- | Parse XML header section
parseHeader :: Parser Header
parseHeader = do
    _ <- stringP "<header"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    _ <- stringP ">"
    skipWhitespace
    _ <- stringP "</header>"
    let title = findAttr "title" attrs ""
    let author = lookup "author" attrs
    let date = lookup "date" attrs
    return (Header title author date)

-- | Parse attributes in an XML tag
parseAttributes :: Parser [(String, String)]
parseAttributes = manyP parseAttribute

-- | Parse a single attribute (name="value")
parseAttribute :: Parser (String, String)
parseAttribute = do
    skipWhitespace
    name <- parseAttributeName
    skipWhitespace
    _ <- char '='
    skipWhitespace
    _ <- char '"'
    value <- parseAttributeValue
    _ <- char '"'
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
parseParagraph = do
    skipWhitespace
    _ <- stringP "<paragraph>"
    inlines <- parseInlines
    _ <- stringP "</paragraph>"
    return (Paragraph inlines)

-- | Parse a section element
parseSection :: Parser Content
parseSection = do
    skipWhitespace
    _ <- stringP "<section"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    _ <- stringP ">"
    skipWhitespace
    contents <- parseContents
    skipWhitespace
    _ <- stringP "</section>"
    return (Section (lookup "title" attrs) contents)

-- | Parse a code block element
parseCodeBlock :: Parser Content
parseCodeBlock = do
    skipWhitespace
    _ <- stringP "<codeblock>"
    code <- parseCodeContent
    _ <- stringP "</codeblock>"
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
parseList = do
    skipWhitespace
    _ <- stringP "<list>"
    skipWhitespace
    items <- manyP parseListItem
    skipWhitespace
    _ <- stringP "</list>"
    return (List items)

-- | Parse a list item
parseListItem :: Parser ListItem
parseListItem = do
    skipWhitespace
    _ <- stringP "<item>"
    inlines <- parseInlines
    _ <- stringP "</item>"
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
parseBold = do
    _ <- stringP "<bold>"
    inlines <- parseInlines
    _ <- stringP "</bold>"
    return (Bold inlines)

-- | Parse italic text
parseItalic :: Parser Inline
parseItalic = do
    _ <- stringP "<italic>"
    inlines <- parseInlines
    _ <- stringP "</italic>"
    return (Italic inlines)

-- | Parse code inline
parseCode :: Parser Inline
parseCode = do
    _ <- stringP "<code>"
    code <- manyP (satisfy (/= '<'))
    _ <- stringP "</code>"
    return (Code code)

-- | Parse link
parseLink :: Parser Inline
parseLink = do
    _ <- stringP "<link"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    _ <- stringP ">"
    text <- manyP (satisfy (/= '<'))
    _ <- stringP "</link>"
    let url = findAttr "href" attrs ""
    return (Link text url)

-- | Parse image
parseImage :: Parser Inline
parseImage = do
    _ <- stringP "<image"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    _ <- stringP "></image>"
    let alt = findAttr "alt" attrs ""
    let src = findAttr "src" attrs ""
    return (Image alt src)