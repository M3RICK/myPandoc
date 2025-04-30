{-
-- EPITECH PROJECT, 2024
-- MarkdownOut.hs
-- File description:
-- Converts Document to Markdown format
-}

module MarkdownOut
( documentToMarkdown
) where

import Document

-- | Convert a Document into a Markdown string
documentToMarkdown :: Document -> String
documentToMarkdown (Document hdr body) =
    headerToMarkdown hdr ++ "\n\n" ++
    concatMap contentToMarkdown body

-- | Convert Header to Markdown
headerToMarkdown :: Header -> String
headerToMarkdown (Header title author date) =
    "---\n" ++
    "title: " ++ title ++ "\n" ++
    maybe "" (\a -> "author: " ++ a ++ "\n") author ++
    maybe "" (\d -> "date: " ++ d ++ "\n") date ++
    "---"

-- | Convert Content to Markdown
contentToMarkdown :: Content -> String
contentToMarkdown (Paragraph inlines) =
    concatMap inlineToMarkdown inlines ++ "\n\n"
contentToMarkdown (Section title contents) =
    case title of
        Just t -> "## " ++ t ++ "\n\n" ++ concatMap contentToMarkdown contents
        Nothing -> concatMap contentToMarkdown contents
contentToMarkdown (CodeBlock code) =
    "```\n" ++ code ++ "\n```\n\n"
contentToMarkdown (List items) =
    concatMap listItemToMarkdown items ++ "\n"

-- | Convert Inline to Markdown
inlineToMarkdown :: Inline -> String
inlineToMarkdown (PlainText text) = text
inlineToMarkdown (Bold inlines) = 
    "**" ++ concatMap inlineToMarkdown inlines ++ "**"
inlineToMarkdown (Italic inlines) = 
    "*" ++ concatMap inlineToMarkdown inlines ++ "*"
inlineToMarkdown (Code text) = "`" ++ text ++ "`"
inlineToMarkdown (Link text url) = "[" ++ text ++ "](" ++ url ++ ")"
inlineToMarkdown (Image alt url) = "![" ++ alt ++ "](" ++ url ++ ")"

-- | Convert ListItem to Markdown
listItemToMarkdown :: ListItem -> String
listItemToMarkdown (ListItem inlines) =
    "- " ++ concatMap inlineToMarkdown inlines ++ "\n"