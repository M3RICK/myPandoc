{-
-- EPITECH PROJECT, 2024
-- XmlOut.hs
-- File description:
-- Converts Document to XML format
-}

module XmlOut
    ( documentToXml
    ) where

import Document

-- | Convert a Document into an XML string
documentToXml :: Document -> String
documentToXml (Document hdr body) =
    "<document>\n" ++
    headerToXml hdr ++ "\n" ++
    "<body>\n" ++
    concatMap contentToXml body ++
    "</body>\n" ++
    "</document>"

-- | Convert Header to XML
headerToXml :: Header -> String
headerToXml (Header title author date) =
    "<header" ++
    " title=\"" ++ escapeXml title ++ "\"" ++
    maybe "" (\a -> " author=\"" ++ escapeXml a ++ "\"") author ++
    maybe "" (\d -> " date=\"" ++ escapeXml d ++ "\"") date ++
    "></header>"

-- | Convert Content to XML
contentToXml :: Content -> String
contentToXml (Paragraph inlines) =
    "<paragraph>" ++ concatMap inlineToXml inlines ++ "</paragraph>"
contentToXml (Section title contents) =
    "<section" ++
    maybe "" (\t -> " title=\"" ++ escapeXml t ++ "\"") title ++
    ">" ++ concatMap contentToXml contents ++ "</section>"
contentToXml (CodeBlock code) =
    "<codeblock>" ++ escapeXml code ++ "</codeblock>"
contentToXml (List items) =
    "<list>" ++ concatMap listItemToXml items ++ "</list>"

-- | Convert Inline to XML
inlineToXml :: Inline -> String
inlineToXml (PlainText text) = escapeXml text
inlineToXml (Bold inlines) = "<bold>" ++ concatMap inlineToXml inlines ++ "</bold>"
inlineToXml (Italic inlines) = "<italic>" ++ concatMap inlineToXml inlines ++ "</italic>"
inlineToXml (Code text) = "<code>" ++ escapeXml text ++ "</code>"
inlineToXml (Link text url) = "<link href=\"" ++ escapeXml url ++ "\">" ++ escapeXml text ++ "</link>"
inlineToXml (Image alt url) = "<image src=\"" ++ escapeXml url ++ "\" alt=\"" ++ escapeXml alt ++ "\"></image>"

-- | Convert ListItem to XML
listItemToXml :: ListItem -> String
listItemToXml (ListItem inlines) =
    "<item>" ++ concatMap inlineToXml inlines ++ "</item>"

-- | Escape special XML characters
escapeXml :: String -> String
escapeXml = concatMap escapeChar
  where
    escapeChar '<' = "<"
    escapeChar '>' = ">"
    escapeChar '&' = "&amp;"
    escapeChar c   = [c]