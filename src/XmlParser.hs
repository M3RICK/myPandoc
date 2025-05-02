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
    parseHeaderTag
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    headerElements <- parseHeaderElements
    let title' = findAttr "title" attrs ""
    return (Header title' (fst headerElements) (snd headerElements))

parseHeaderTag::Parser ()
parseHeaderTag =
    skipWhitespace >>
    stringP "<header" >>
    skipWhitespace >>
    return ()

parseHeaderElements::Parser (Maybe String, Maybe String)
parseHeaderElements = do
    skipWhitespace
    authorOpt <- optionP Nothing parseAuthorElement
    skipWhitespace
    dateOpt <- optionP Nothing parseDateElement
    skipWhitespace
    stringP "</header>"
    return (authorOpt, dateOpt)

parseAuthorElement::Parser (Maybe String)
parseAuthorElement = do
    stringP "<author>"
    author <- manyP (satisfy (/= '<'))
    stringP "</author>"
    return (Just author)

parseDateElement::Parser (Maybe String)
parseDateElement = do
    stringP "<date>"
    date <- manyP (satisfy (/= '<'))
    stringP "</date>"
    return (Just date)

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
parseContent = parseParagraph `orElse`
              parseSection `orElse`
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
    skipWhitespace
    codeContents <- manyP (parseContent <* skipWhitespace)
    stringP "</codeblock>"
    let codeStr = contentToString codeContents
    return (CodeBlock codeStr)

contentToString::[Content] -> String
contentToString [] = ""
contentToString (Paragraph inlines:rest) =
    concatMap inlineToString inlines ++ "\n" ++ contentToString rest
contentToString (_:rest) = contentToString rest

inlineToString::Inline -> String
inlineToString (PlainText text) = text
inlineToString (Bold inlines) = concatMap inlineToString inlines
inlineToString (Italic inlines) = concatMap inlineToString inlines
inlineToString (Code text) = text
inlineToString (Link text _) = text
inlineToString (Image alt _) = alt

parseList::Parser Content
parseList = do
    skipWhitespace
    stringP "<list>"
    skipWhitespace
    paragraphs <- manyP (parseParagraph <* skipWhitespace)
    let items = map paragraphToListItem paragraphs
    stringP "</list>"
    return (List items)

paragraphToListItem::Content -> ListItem
paragraphToListItem (Paragraph inlines) = ListItem inlines
paragraphToListItem _ = ListItem [PlainText ""]

parseInlines::Parser [Inline]
parseInlines = manyP parseInline

parseInline::Parser Inline
parseInline = parsePlainText `orElse` parseFormattedInline

parseFormattedInline::Parser Inline
parseFormattedInline =
    parseBold `orElse`
    parseItalic `orElse`
    parseCode `orElse`
    parseEPITECHLink `orElse`
    parseEPITECHImage

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

parseEPITECHLink::Parser Inline
parseEPITECHLink = do
    stringP "<link"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    text <- manyP (satisfy (/= '<'))
    stringP "</link>"
    let url = findAttr "url" attrs ""
    return (Link text url)

parseEPITECHImage::Parser Inline
parseEPITECHImage = do
    stringP "<image"
    skipWhitespace
    attrs <- parseAttributes
    skipWhitespace
    char '>'
    alt <- manyP (satisfy (/= '<'))
    stringP "</image>"
    let url = findAttr "url" attrs ""
    return (Image alt url)
