{-
-- EPITECH PROJECT, 2024
-- JsonOut.hs
-- File description:
-- Converts Document to JSON format
-}

module JsonOut
( documentToJson
) where

import Document

concatMapWithSeparator::(a -> String) -> [a] -> String
concatMapWithSeparator _ [] = ""
concatMapWithSeparator f [x] = f x
concatMapWithSeparator f (x:xs) =
    f x ++ ",\n          " ++ concatMapWithSeparator f xs

documentToJson::Document -> String
documentToJson (Document hdr body) =
    "{\n" ++
    "  \"header\": " ++ headerToJson hdr ++ ",\n" ++
    "  \"body\": [\n    " ++
    concatMapWithSeparator contentToJson body ++
    "\n  ]\n" ++
    "}"

headerToJson::Header -> String
headerToJson header =
    "{\n" ++
    formatHeaderFields header ++
    "\n  }"

-- | Format header fields
formatHeaderFields::Header -> String
formatHeaderFields (Header title author date) =
    "    \"title\": \"" ++ escapeJson title ++ "\"" ++
    addAuthor author ++
    addDate date
  where
    addAuthor Nothing = ""
    addAuthor (Just a) = ",\n    \"author\": \"" ++ escapeJson a ++ "\""
    addDate Nothing = ""
    addDate (Just d) = ",\n    \"date\": \"" ++ escapeJson d ++ "\""

contentToJson::Content -> String
contentToJson (Paragraph inlines) = formatParagraph inlines
contentToJson (Section title contents) = formatSection title contents
contentToJson (CodeBlock code) = formatCodeBlock code
contentToJson (List items) = formatList items

formatParagraph::[Inline] -> String
formatParagraph inlines =
    "{\n      \"paragraph\": [" ++
    inlinesToJson inlines ++
    "]\n    }"

formatSection::Maybe String -> [Content] -> String
formatSection title contents =
    "{\n      \"section\": {\n" ++
    "        \"title\": " ++
    formatTitle title ++
    ",\n" ++
    formatSectionContent contents ++
    "\n      }\n    }"
  where
    formatTitle Nothing = "null"
    formatTitle (Just t) = "\"" ++ escapeJson t ++ "\""

formatSectionContent::[Content] -> String
formatSectionContent contents =
    "        \"content\": [\n          " ++
    concatMapWithSeparator contentToJson contents ++
    "\n        ]"

formatCodeBlock::String -> String
formatCodeBlock code =
    "{\n      \"codeblock\": \"" ++
    escapeJson code ++
    "\"\n    }"

formatList::[ListItem] -> String
formatList items =
    "{\n      \"list\": [\n        " ++
    concatMapWithSeparator listItemToJson items ++
    "\n      ]\n    }"

inlinesToJson::[Inline] -> String
inlinesToJson [] = ""
inlinesToJson [x] = inlineToJson x
inlinesToJson (x:xs) = inlineToJson x ++ ", " ++ inlinesToJson xs

inlineToJson::Inline -> String
inlineToJson (PlainText text) = "\"" ++ escapeJson text ++ "\""
inlineToJson (Bold inlines) =
    "{\n        \"bold\": [" ++
    inlinesToJson inlines ++
    "]\n      }"
inlineToJson (Italic inlines) =
    "{\n        \"italic\": [" ++
    inlinesToJson inlines ++
    "]\n      }"
inlineToJson (Code text) =
    "{\n        \"code\": \"" ++
    escapeJson text ++
    "\"\n      }"
inlineToJson (Link text url) =
    "{\n        \"link\": {\n" ++
    "          \"text\": \"" ++ escapeJson text ++ "\",\n" ++
    "          \"url\": \"" ++ escapeJson url ++ "\"\n" ++
    "        }\n      }"
inlineToJson (Image alt url) =
    "{\n        \"image\": {\n" ++
    "          \"alt\": \"" ++ escapeJson alt ++ "\",\n" ++
    "          \"url\": \"" ++ escapeJson url ++ "\"\n" ++
    "        }\n      }"

listItemToJson::ListItem -> String
listItemToJson (ListItem inlines) =
    "{\n          \"item\": [" ++
    inlinesToJson inlines ++
    "]\n        }"

escapeJson::String -> String
escapeJson = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]