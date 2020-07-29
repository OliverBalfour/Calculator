
module ParserCombinator where

import Control.Monad (ap)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isSpace, isDigit, ord)
import GHC.Float (int2Double)

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

-- Parsers are monads so we can sequentially apply parsers neatly
instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f = Parser $ \cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs]

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Functor Parser where
  fmap f x = pure f <*> x

-- Parsers are monoids where mappend ++'s results from two parsers
instance Semigroup (Parser a) where
  p <> q = Parser (\cs -> parse p cs ++ parse q cs)

instance Monoid (Parser a) where
  mempty = Parser (\_ -> [])
  mappend = (<>)

-- Parsers are alternative so p <|> q applies q only if p fails
-- This lets us try multiple parsers sequentially
instance Alternative Parser where
  empty = mempty
  p <|> q = Parser (\cs ->
    let p' = parse p cs in
    case p' of
      [] -> parse q cs
      _  -> p')

-- match a single character that satisfies a predicate function
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  c <- item
  if predicate c
    then return c
    else mempty

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string "" = return ""
string (c:cs) = (:) <$> char c <*> string cs

-- repeated applications of a parser
many :: Parser a -> Parser [a]
many p = many1 p <|> return []
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- parse a list [a] delimited by separators b
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> empty
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = (:) <$> p <*> many (sep *> p)

-- chain left/right associative binary operators
-- eg for int addition: chainl (many1 $ char isDigit) (char "+") (return 0)
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op fallback = (p `chainl1` op) <|> (return fallback)
chainr p op fallback = (p `chainr1` op) <|> (return fallback)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a
p `chainr1` op = do { a <- p; rest a }
  where rest a =  (op <*> pure a <*> (p >>= rest)) <|> return a

space :: Parser String
space = many (satisfy isSpace)

-- parse a string token and discard trailing spaces
symb :: String -> Parser String
symb cs = string cs <* space

digit :: Parser Int
digit = fmap (\c -> ord c - ord '0') (satisfy isDigit)

positiveInteger :: Parser Int
positiveInteger = fmap base10 (many1 digit)
  where base10 ns = sum $ zipWith (\a b -> 10 ^ a * b) [0..] (reverse ns)

integer :: Parser Int
integer = (char '-' *> fmap negate positiveInteger) <|> positiveInteger

floatingPoint :: Parser Double
floatingPoint = (do
  first <- integer <|> return 0
  let first' = int2Double first
  point <- char '.'
  second <- integer <|> return 0
  let second' = (int2Double second) / ((10**) . int2Double . length . show $ second)
  return (first' + second')) <|> (fmap int2Double integer)

-- caveat: the exponent is an int, so if there is a decimal point after,
-- it is implicitly multiplied (eg 5e-2.35 = 5*10**(-2) * 0.35)
floatingPointExponent :: Parser Double
floatingPointExponent = (do
  x <- floatingPoint
  char 'e'
  e <- floatingPoint
  return (x * 10**e)) <|> floatingPoint

number :: Parser Double
number = floatingPointWithExponent <* space

-- apply a parser and discard leading spaces
apply :: Parser a -> String -> [(a, String)]
apply p = parse (space *> p)

-- apply a parser only if a different parser would fail, without consuming the string
-- ex: (notahead (symb "++")) *> symb "+"  matches + only if there is no ++
notahead :: Parser a -> Parser ()
notahead p = Parser (\cs -> do
  if (length (apply p cs) == 0)
    then return ((), cs)
    else mempty)
