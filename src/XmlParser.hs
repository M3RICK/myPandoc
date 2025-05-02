{-
-- EPITECH PROJECT, 2024
-- XmlParser.hs
-- File description:
-- XML Parser implementation for document conversion (school format compatible)
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
    header' <- parseSchoolHeader
    skipWhitespace
    content' <- parseDocumentBody
    skipWhitespace
    stringP "</document>"
    skipWhitespace
    return (Document header' content')

-- | Parse header in school format (with nested author and date)
parseSchoolHeader :: Parser Header
parseSchoolHeader = do
    skipWhitespace
    stringP "<header"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    skipWhitespace

    -- Try to parse author element
    authorOpt <- optionP Nothing (parseAuthorElement)
    skipWhitespace

    -- Try to parse date element
    dateOpt <- optionP Nothing (parseDateElement)
    skipWhitespace

    stringP "</header>"

    -- Get title from attributes
    let title' = findAttr "title" attrs ""

    return (Header title' authorOpt dateOpt)

-- | Parse author element
parseAuthorElement :: Parser (Maybe String)
parseAuthorElement = do
    stringP "<author>"
    author <- manyP (satisfy (/= '<'))
    stringP "</author>"
    return (Just author)

-- | Parse date element
parseDateElement :: Parser (Maybe String)
parseDateElement = do
    stringP "<date>"
    date <- manyP (satisfy (/= '<'))
    stringP "</date>"
    return (Just date)

parseDocumentBody :: Parser [Content]
parseDocumentBody = do
    stringP "<body>"
    skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
    stringP "</body>"
    return contents

parseAttributes :: Parser [(String, String)]
parseAttributes = manyP parseAttribute

parseAttribute :: Parser (String, String)
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

isAttributeNameChar :: Char -> Bool
isAttributeNameChar c = isAlphaNum c || c == '_' || c == '-'

parseAttributeName :: Parser String
parseAttributeName = someP (satisfy isAttributeNameChar)

findAttr :: String -> [(String, String)] -> String -> String
findAttr name attrs defaultValue =
    case lookup name attrs of
        Just value -> value
        Nothing -> defaultValue

parseContent :: Parser Content
parseContent = parseParagraph `orElse`
              parseSection `orElse`
              parseCodeBlock `orElse`
              parseList

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
    char '>'
    skipWhitespace
    contents <- manyP (parseContent <* skipWhitespace)
    stringP "</section>"
    return (Section (lookup "title" attrs) contents)

-- | Parse a code block element
parseCodeBlock :: Parser Content
parseCodeBlock = do
    skipWhitespace
    stringP "<codeblock>"
    skipWhitespace
    codeContents <- manyP (parseContent <* skipWhitespace)
    stringP "</codeblock>"
    let codeStr = contentToString codeContents
    return (CodeBlock codeStr)

-- | Convert content to string representation
contentToString :: [Content] -> String
contentToString [] = ""
contentToString (Paragraph inlines:rest) =
    concatMap inlineToString inlines ++ "\n" ++ contentToString rest
contentToString (_:rest) = contentToString rest

-- | Convert inline to string
inlineToString :: Inline -> String
inlineToString (PlainText text) = text
inlineToString (Bold inlines) = concatMap inlineToString inlines
inlineToString (Italic inlines) = concatMap inlineToString inlines
inlineToString (Code text) = text
inlineToString (Link text _) = text
inlineToString (Image alt _) = alt

parseList :: Parser Content
parseList = do
    skipWhitespace
    stringP "<list>"
    skipWhitespace
    paragraphs <- manyP (parseParagraph <* skipWhitespace)
    let items = map paragraphToListItem paragraphs
    stringP "</list>"
    return (List items)

paragraphToListItem :: Content -> ListItem
paragraphToListItem (Paragraph inlines) = ListItem inlines
paragraphToListItem _ = ListItem [PlainText ""]

parseInlines :: Parser [Inline]
parseInlines = manyP parseInline

parseInline :: Parser Inline
parseInline = parsePlainText `orElse` parseFormattedInline

parseFormattedInline :: Parser Inline
parseFormattedInline =
    parseBold `orElse`
    parseItalic `orElse`
    parseCode `orElse`
    parseSchoolLink `orElse`
    parseSchoolImage

parsePlainText :: Parser Inline
parsePlainText = Parser $ \input ->
    let (text, rest) = span (/= '<') input
    in if null text then Nothing else Just (PlainText text, rest)

parseBold :: Parser Inline
parseBold = do
    stringP "<bold>"
    inlines <- parseInlines
    stringP "</bold>"
    return (Bold inlines)

parseItalic :: Parser Inline
parseItalic = do
    stringP "<italic>"
    inlines <- parseInlines
    stringP "</italic>"
    return (Italic inlines)

parseCode :: Parser Inline
parseCode = do
    stringP "<code>"
    text <- manyP (satisfy (/= '<'))
    stringP "</code>"
    return (Code text)

-- | Parse link (school format with url attribute)
parseSchoolLink :: Parser Inline
parseSchoolLink = do
    stringP "<link"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    text <- manyP (satisfy (/= '<'))
    stringP "</link>"
    let url = findAttr "url" attrs ""
    return (Link text url)

-- | Parse image (school format with content and url attribute)
parseSchoolImage :: Parser Inline
parseSchoolImage = do
    stringP "<image"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    alt <- manyP (satisfy (/= '<'))
    stringP "</image>"
    let url = findAttr "url" attrs ""
    return (Image alt url)
