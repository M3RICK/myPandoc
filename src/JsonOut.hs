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

-- | Convert a Document into a JSON string
documentToJson :: Document -> String
documentToJson (Document hdr body) =
    "{\n" ++
    "  \"header\": " ++ headerToJson hdr ++ ",\n" ++
    "  \"body\": [\n    " ++
    concatMap contentToJson body ++
    "\n  ]\n" ++
    "}"

-- | Convert Header to JSON
headerToJson :: Header -> String
headerToJson (Header title author date) =
    "{\n" ++
    "    \"title\": \"" ++ escapeJson title ++ "\"" ++
    maybe "" (\a -> ",\n    \"author\": \"" ++ escapeJson a ++ "\"") author ++
    maybe "" (\d -> ",\n    \"date\": \"" ++ escapeJson d ++ "\"") date ++
    "\n  }"

-- | Convert Content to JSON with proper indentation
-- Fix: Remove trailing commas that make the JSON invalid
contentToJson :: Content -> String
contentToJson (Paragraph inlines) =
    "{\n      \"paragraph\": [" ++
    inlinesToJson inlines ++
    "]\n    }" -- Removed comma
contentToJson (Section title contents) =
    "{\n      \"section\": {\n" ++
    "        \"title\": " ++
    maybe "null" (\t -> "\"" ++ escapeJson t ++ "\"") title ++
    ",\n" ++
    "        \"content\": [\n          " ++
    concatMapWithSeparator contentToJson contents ++
    "\n        ]\n      }\n    }" -- Removed comma
contentToJson (CodeBlock code) =
    "{\n      \"codeblock\": \"" ++ escapeJson code ++ "\"\n    }" -- Removed comma
contentToJson (List items) =
    "{\n      \"list\": [\n        " ++
    concatMapWithSeparator listItemToJson items ++
    "\n      ]\n    }" -- Removed comma

-- | Helper function to join items with commas but no trailing comma
concatMapWithSeparator :: (a -> String) -> [a] -> String
concatMapWithSeparator _ [] = ""
concatMapWithSeparator f [x] = f x
concatMapWithSeparator f (x:xs) = f x ++ ",\n          " ++ concatMapWithSeparator f xs

-- | Convert a list of Inline elements to JSON array
inlinesToJson :: [Inline] -> String
inlinesToJson [] = ""
inlinesToJson [x] = inlineToJson x
inlinesToJson (x:xs) = inlineToJson x ++ ", " ++ inlinesToJson xs

-- | Convert Inline to JSON
inlineToJson :: Inline -> String
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

-- | Convert ListItem to JSON
-- Fix: Remove trailing comma
listItemToJson :: ListItem -> String
listItemToJson (ListItem inlines) =
    "{\n          \"item\": [" ++
    inlinesToJson inlines ++
    "]\n        }" -- Removed comma

-- | Escape special JSON characters
escapeJson :: String -> String
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
