{-
-- EPITECH PROJECT, 2024
-- ParsingLibrary.hs
-- File description:
-- Its parsin time
-}

--  A NOTER QUE P A LA FIN DE CHAQUE MERDE C EST POUR PARSER par exemple ma biche manyP, many PARSEEEEEEEEEEEEEER

module ParsingLibrary
  ( ParseResult
  , Parser(..)
  , run
  , pureP
  , emptyP
  , thenP
  , currentInput

  , char
  , oneOf
  , noneOf
  , satisfy

  , orElse
  , pairP
  , mapP
  , map2P
  , manyP
  , someP
  , optionP

  , stringP
  , digit
  , letter
  , alphaNum
  , space
  , skipSpaces
  , token
  , oneOfStr
  , noneOfStr
  , whitespaces
  , skipWhitespace
  , word
  , exactWord

  , natural
  , intingeger
  , tupleP
  ) where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

-- | Type de retour d'un parseur : soit une valeur avec le reste de l'entrée, soit échec
type ParseResult a = Maybe (a, String)

-- | Type principal : un parseur est une fonction sur une String
newtype Parser a = Parser { parse::String -> ParseResult a }

-- | Lance un parseur
run::Parser a -> String -> ParseResult a
run (Parser p) = p

-- | Parseur qui réussit toujours avec une valeur donnée
pureP::a -> Parser a
pureP x = Parser $ \s -> Just (x, s)

-- | Parseur qui échoue toujours
emptyP::Parser a
emptyP = Parser $ \_ -> Nothing

-- | Enchaîne deux parseurs avec transformation intermédiaire
thenP::Parser a -> (a -> String -> ParseResult b) -> Parser b
thenP (Parser p) f = Parser $ \s -> case p s of
  Nothing -> Nothing
  Just (a, rest) -> f a rest

-- | Récupère l'entrée actuelle sans la consommer
currentInput::Parser String
currentInput = Parser $ \s -> Just (s, s)

-- | Accepte un caractère précis
char::Char -> Parser Char
char expected = Parser $ \s -> case s of
  (c:cs) | c == expected -> Just (c, cs)
  _ -> Nothing

-- | Accepte un caractère dans une liste
oneOf::[Char] -> Parser Char
oneOf chars = Parser $ \s -> case s of
  (c:cs) | c `elem` chars -> Just (c, cs)
  _ -> Nothing

-- | Rejette un caractère s'il est dans la liste
noneOf::[Char] -> Parser Char
noneOf chars = Parser $ \s -> case s of
  (c:cs) | c `notElem` chars -> Just (c, cs)
  _ -> Nothing

-- | Accepte un caractère selon un prédicat
satisfy::(Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  (c:cs) | f c -> Just (c, cs)
  _ -> Nothing

-- | Essaie deux parseurs, retourne le premier qui réussit
orElse::Parser a -> Parser a -> Parser a
orElse p1 p2 = Parser $ \s -> case run p1 s of
  Nothing -> run p2 s
  success -> success

-- | Enchaîne deux parseurs, retourne un tuple
pairP::Parser a -> Parser b -> Parser (a, b)
pairP p1 p2 = thenP p1 $ \a rest1 ->
  case run p2 rest1 of
    Nothing -> Nothing
    Just (b, rest2) -> Just ((a, b), rest2)

-- | Applique une fonction sur le résultat d'un parseur
mapP::(a -> b) -> Parser a -> Parser b
mapP f p = thenP p $ \a rest -> Just (f a, rest)

-- | Combine deux parseurs avec une fonction
map2P::(a -> b -> c) -> Parser a -> Parser b -> Parser c
map2P f p1 p2 = thenP p1 $ \a rest1 ->
  case run p2 rest1 of
    Nothing -> Nothing
    Just (b, rest2) -> Just (f a b, rest2)

-- | Répète un parseur autant que possible (0 ou plus)
manyP::Parser a -> Parser [a]
manyP p = Parser $ \s ->
  let go acc input = case run p input of
        Nothing -> Just (acc, input)
        Just (x, rest) -> go (acc ++ [x]) rest
  in go [] s

-- | Répète un parseur au moins une fois
someP::Parser a -> Parser [a]
someP p = thenP p $ \x rest1 ->
  case run (manyP p) rest1 of
    Nothing -> Just ([x], rest1)
    Just (xs, rest2) -> Just (x:xs, rest2)

-- | Rend un parseur facultatif avec valeur par défaut
optionP::a -> Parser a -> Parser a
optionP def p = orElse p (pureP def)

-- | Parse une chaîne exacte
stringP::String -> Parser String
stringP target = Parser $ \s ->
  let len = length target
      (prefix, suffix) = splitAt len s
  in if prefix == target then Just (target, suffix) else Nothing

-- | Caractère chiffre
digit::Parser Char
digit = satisfy isDigit

-- | Caractère lettre
letter::Parser Char
letter = satisfy isAlpha

-- | Caractère alphanumérique
alphaNum::Parser Char
alphaNum = satisfy isAlphaNum

-- | Caractère d’espace
space::Parser Char
space = satisfy isSpace

-- | Ignores espaces
skipSpaces::Parser String
skipSpaces = manyP space

-- | Applique un parseur puis saute les espaces
token::Parser a -> Parser a
token p = map2P const p skipSpaces

-- | Accepte un caractère d’une chaîne
oneOfStr::String -> Parser Char
oneOfStr = oneOf

-- | Rejette un caractère d’une chaîne
noneOfStr::String -> Parser Char
noneOfStr = noneOf

-- | Au moins un caractère blanc
whitespaces::Parser String
whitespaces = someP (oneOf " \t\n\r")

-- | Zéro ou plusieurs blancs
skipWhitespace::Parser String
skipWhitespace = manyP (oneOf " \t\n\r")

-- | Mot = lettre suivie de caractères alphanumériques
word::Parser String
word = map2P (:) letter (manyP alphaNum)

-- | Mot-clé exact, non suivi d’un mot
exactWord::String -> Parser String
exactWord kw = thenP (stringP kw) $ \res rest ->
  case rest of
    (c:_) | isAlphaNum c -> Nothing
    _ -> Just (res, rest)

-- | Entier naturel (positif)
natural::Parser Int
natural = mapP read (someP digit)

-- | Entier signé (+ ou -)
intingeger::Parser Int -- inting tousse tousse you get it thierry
intingeger = orElse
  (thenP (char '-') $ \_ r -> case run natural r of
    Just (n, rest) -> Just (-n, rest)
    Nothing -> Nothing)
  natural

-- | Tuple (a,b) entre parenthèses
tupleP::Parser a -> Parser (a, a)
tupleP p =
  thenP (char '(') $ \_ s1 ->
  thenP p $ \x s2 ->
  thenP (char ',') $ \_ s3 ->
  thenP p $ \y s4 ->
  thenP (char ')') $ \_ s5 ->
    Just ((x, y), s5)
