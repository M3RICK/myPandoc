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
parseXml =
    skipWhitespace >>
    stringP "<document>" >>
    parseDocumentContent

-- | Parse document content (after opening tag)
parseDocumentContent :: Parser Document
parseDocumentContent = do
    skipWhitespace
    header' <- parseHeader
    skipWhitespace
    content' <- parseDocumentBody
    parseDocumentEnd
    return (Document header' content')

-- | Parse document end tag
parseDocumentEnd :: Parser ()
parseDocumentEnd =
    skipWhitespace >>
    stringP "</document>" >>
    skipWhitespace >>
    return ()

-- | Parse document body
parseDocumentBody :: Parser [Content]
parseDocumentBody = do
    stringP "<body>"
    skipWhitespace
    contents <- parseBodyContents
    stringP "</body>"
    return contents

-- | Parse body contents
parseBodyContents :: Parser [Content]
parseBodyContents = manyP (parseContent <* skipWhitespace)

-- | Create Header from attributes
createHeader :: [(String, String)] -> Header
createHeader attrs =
    let title' = findAttr "title" attrs ""
        author' = lookup "author" attrs
        date' = lookup "date" attrs
    in Header title' author' date'

-- | Parse XML header tag and contents
parseHeader :: Parser Header
parseHeader =
    skipWhitespace >>
    stringP "<header" >>
    parseHeaderAttributes

-- | Parse header attributes and closing tag
parseHeaderAttributes :: Parser Header
parseHeaderAttributes = do
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
    parseAttributeEquals name

-- | Parse the equals sign and value of an attribute
parseAttributeEquals :: String -> Parser (String, String)
parseAttributeEquals name = do
    skipWhitespace
    char '='
    skipWhitespace
    parseAttributeValue name

-- | Parse attribute value in quotes
parseAttributeValue :: String -> Parser (String, String)
parseAttributeValue name = do
    char '"'
    value <- manyP (satisfy (/= '"'))
    char '"'
    return (name, value)

-- | Is character valid in an attribute name?
isAttributeNameChar :: Char -> Bool
isAttributeNameChar c = isAlphaNum c || c == '_' || c == '-'

-- | Parse attribute name
parseAttributeName :: Parser String
parseAttributeName = someP (satisfy isAttributeNameChar)

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

-- | Parse a section element - opening tag and attributes
parseSection :: Parser Content
parseSection = do
    skipWhitespace
    stringP "<section"
    skipWhitespace
    attrs <- parseAttributes
    parseSectionContent attrs

-- | Parse section content and closing tag
parseSectionContent :: [(String, String)] -> Parser Content
parseSectionContent attrs = do
    skipWhitespace
    stringP ">"
    skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
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
    let (content, rest) = breakAtTag input endTag
    in if take (length endTag) rest == endTag
       then Just (content, rest)
       else Nothing

-- | Helper to break a string at a tag
breakAtTag :: String -> String -> (String, String)
breakAtTag input tag =
    let idx = findTagIndex input tag 0
    in splitAt idx input

-- | Find index of a tag in a string
findTagIndex :: String -> String -> Int -> Int
findTagIndex [] _ _ = 0
findTagIndex str tag idx
    | take (length tag) str == tag = idx
    | null str = 0
    | otherwise = findTagIndex (tail str) tag (idx + 1)

-- | Parse a list element
parseList :: Parser Content
parseList = do
    skipWhitespace
    stringP "<list>"
    skipWhitespace
    parseListItems

-- | Parse list items and closing tag
parseListItems :: Parser Content
parseListItems = do
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
parseInline = parsePlainText `orElse` parseFormattedInline

-- | Parse formatted inline elements
parseFormattedInline :: Parser Inline
parseFormattedInline =
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

-- | Parse link opening tag and attributes
parseLink :: Parser Inline
parseLink = do
    stringP "<link"
    skipWhitespace
    attrs <- parseAttributes
    parseLinkContent attrs

-- | Parse link content and closing tag
parseLinkContent :: [(String, String)] -> Parser Inline
parseLinkContent attrs = do
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
    parseImageEnd attrs

-- | Parse image end tag
parseImageEnd :: [(String, String)] -> Parser Inline
parseImageEnd attrs = do
    skipWhitespace
    stringP "></image>"
    let alt = findAttr "alt" attrs ""
        src = findAttr "src" attrs ""
    return (Image alt src)
