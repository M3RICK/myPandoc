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
import Data.Char (isAlphaNum)

-- | Parse a complete XML document
parseXml :: Parser Document
parseXml = do
    skipWhitespace
    stringP "<document>"
    skipWhitespace
    header' <- parseHeader
    skipWhitespace
    content' <- parseDocumentBody
    skipWhitespace
    stringP "</document>"
    skipWhitespace
    return (Document header' content')

-- | Parse document body
parseDocumentBody :: Parser [Content]
parseDocumentBody = do
    stringP "<body>"
    skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
    stringP "</body>"
    return contents

-- | Create Header from attributes
createHeader :: [(String, String)] -> Header
createHeader attrs =
    let title' = findAttr "title" attrs ""
        author' = lookup "author" attrs
        date' = lookup "date" attrs
    in Header title' author' date'

-- | Parse XML header tag and contents
parseHeader :: Parser Header
parseHeader = do
    skipWhitespace
    stringP "<header"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    stringP "></header>"
    return (createHeader attrs)

-- | Parse attributes in an XML tag
parseAttributes :: Parser [(String, String)]
parseAttributes = manyP parseAttribute

-- | Parse a single attribute (name="value")
parseAttribute :: Parser (String, String)
parseAttribute = do
    skipWhitespace
    name <- parseAttributeName
    skipWhitespace
    char '='
    skipWhitespace
    char '"'
    value <- parseAttributeValue
    char '"'
    return (name, value)

-- | Is character valid in an attribute name?
isAttributeNameChar :: Char -> Bool
isAttributeNameChar c = isAlphaNum c || c == '_' || c == '-'

-- | Parse attribute name
parseAttributeName :: Parser String
parseAttributeName = someP (satisfy isAttributeNameChar)

-- | Parse attribute value (characters between quotes)
parseAttributeValue :: Parser String
parseAttributeValue = manyP (satisfy (/= '"'))

-- | Get attribute value with default
findAttr :: String -> [(String, String)] -> String -> String
findAttr name attrs defaultValue =
    case lookup name attrs of
        Just value -> value
        Nothing -> defaultValue

-- | Parse a content element
parseContent :: Parser Content
parseContent = parseParagraph `orElse`
              parseSection `orElse`
              parseCodeBlock `orElse`
              parseList

-- | Parse a paragraph element
parseParagraph :: Parser Content
parseParagraph = do
    skipWhitespace
    stringP "<paragraph>"
    inlines <- parseInlines
    stringP "</paragraph>"
    return (Paragraph inlines)

-- | Parse a section element
parseSection :: Parser Content
parseSection = do
    skipWhitespace
    stringP "<section"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    stringP ">"
    skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
    skipWhitespace
    stringP "</section>"
    return (Section (lookup "title" attrs) contents)

-- | Parse a code block element
parseCodeBlock :: Parser Content
parseCodeBlock = do
    skipWhitespace
    stringP "<codeblock>"
    code <- parseTextUntil "</codeblock>"
    stringP "</codeblock>"
    return (CodeBlock code)

-- | Parse text until a specific closing tag
parseTextUntil :: String -> Parser String
parseTextUntil endTag = Parser $ \input ->
    let endTagIndex = findEndTag input 0
        (content, rest) = splitAt endTagIndex input
    in if endTagIndex >= 0 && take (length endTag) rest == endTag
       then Just (content, rest)
       else Nothing
  where
    findEndTag :: String -> Int -> Int
    findEndTag [] _ = -1
    findEndTag str@(c:cs) idx
      | take (length endTag) str == endTag = idx
      | otherwise = findEndTag cs (idx + 1)

-- | Parse a list element
parseList :: Parser Content
parseList = do
    skipWhitespace
    stringP "<list>"
    skipWhitespace
    items <- manyP (parseListItem <* skipWhitespace)
    stringP "</list>"
    return (List items)

-- | Parse a list item
parseListItem :: Parser ListItem
parseListItem = do
    skipWhitespace
    stringP "<item>"
    inlines <- parseInlines
    stringP "</item>"
    return (ListItem inlines)

-- | Parse inline elements
parseInlines :: Parser [Inline]
parseInlines = manyP parseInline

-- | Parse a single inline element
parseInline :: Parser Inline
parseInline =
    parsePlainText `orElse`
    parseBold `orElse`
    parseItalic `orElse`
    parseCode `orElse`
    parseLink `orElse`
    parseImage

-- | Parse plain text
parsePlainText :: Parser Inline
parsePlainText = Parser $ \input ->
    let (text, rest) = span (/= '<') input
    in if null text then Nothing else Just (PlainText text, rest)

-- | Parse bold text
parseBold :: Parser Inline
parseBold = do
    stringP "<bold>"
    inlines <- parseInlines
    stringP "</bold>"
    return (Bold inlines)

-- | Parse italic text
parseItalic :: Parser Inline
parseItalic = do
    stringP "<italic>"
    inlines <- parseInlines
    stringP "</italic>"
    return (Italic inlines)

-- | Parse code inline
parseCode :: Parser Inline
parseCode = do
    stringP "<code>"
    code <- parseTextUntil "</code>"
    stringP "</code>"
    return (Code code)

-- | Parse link
parseLink :: Parser Inline
parseLink = do
    stringP "<link"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    stringP ">"
    text <- parseTextUntil "</link>"
    stringP "</link>"
    let url = findAttr "href" attrs ""
    return (Link text url)

-- | Parse image
parseImage :: Parser Inline
parseImage = do
    stringP "<image"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    stringP "></image>"
    let alt = findAttr "alt" attrs ""
        src = findAttr "src" attrs ""
    return (Image alt src)
