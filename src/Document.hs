module Document
    ( Document(..)
    , Header(..)
    , Content(..)
    , Inline(..)
    , ListItem(..)
    ) where

data Document = Document
    { header :: Header
    , content :: [Content]
    } deriving (Show)

data Header = Header
    { title :: String
    , author :: Maybe String
    , date :: Maybe String
    } deriving (Show)

data Content
    = Paragraph [Inline]
    | Section (Maybe String) [Content]
    | CodeBlock String
    | List [ListItem]
    deriving (Show)

data Inline
    = PlainText String
    | Bold [Inline]
    | Italic [Inline]
    | Code String
    | Link String String
    | Image String String
    deriving (Show)

data ListItem = ListItem [Inline]
    deriving (Show)