
module ParserCombinator where

import Control.Monad (ap)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isSpace, isDigit, ord)
import GHC.Float (int2Double)

newtype Parser a = Parser (String -> [(a, String)])
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

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
    case parse (p <> q) cs of
      []     -> []
      (x:xs) -> [x])

-- match a single character that satisfies a predicate function
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  c <- item
  if predicate c
    then return c
    else mempty

char :: Char -> Parser Char
char c = satisfy $ (==) c

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c:cs)

-- 0 or more applications of a parser
many :: Parser a -> Parser [a]
many p = (many1 p) <|> (return [])

-- 1 or more applications of a parser
many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

-- parse a list [a] delimited by separators b
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> (return [])

-- sepby variant enforcing length >= 1
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  a <- p
  as <- many $ do { sep; p }
  return (a:as)

-- chain left associative binary operators
-- eg for int addition: chainl (many1 $ char isDigit) (char "+") (return 0)
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op fallback = (p `chainl1` op) <|> (return fallback)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a = (do f <- op
                 b <- p
                 rest (f a b))
             <|> return a

space :: Parser String
space = many (satisfy isSpace)

-- parse a string and discard trailing spaces
token :: Parser a -> Parser a
token p = p <* space

digit :: Parser Int
digit = fmap (\c -> ord c - ord '0') (satisfy isDigit)

integer :: Parser Int
integer = fmap base10 (many1 digit) <* space

base10 :: [Int] -> Int
base10 ns = sum $ zipWith (\a b -> 10 ^ a * b) [0..] (reverse ns)

floatingPoint :: Parser Double
floatingPoint = do
  first <- integer
  let first' = int2Double first
  point <- char '.'
  second <- integer
  let second' = (int2Double second) / ((10**) . int2Double . length . show $ second)
  return (first' + second')

number :: Parser Double
number = floatingPoint <|> (fmap int2Double integer)

-- parse a string token
symb :: String -> Parser String
symb cs = token (string cs)

-- apply a parser and discard leading spaces
apply :: Parser a -> String -> [(a, String)]
apply p = parse (space *> p)
