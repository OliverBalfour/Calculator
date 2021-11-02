
{-# LANGUAGE LambdaCase #-}
module ParserCombinator where

import Control.Monad (ap)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isSpace, isDigit, ord)
import Data.Bool (bool)
import Number (Number(NumZ), toR, toDouble)
import GHC.Float.RealFracMethods ( truncateDoubleInteger )

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser (\case
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
  mempty = Parser (const [])
  mappend = (<>)

-- Parsers are alternative so p <|> q applies q only if p fails
-- This lets us try multiple parsers sequentially
instance Alternative Parser where
  empty = mempty
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if not (null p') then p' else q')

-- match a single character that satisfies a predicate function
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = item >>= (\c -> if predicate c then return c else mempty)

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

repeatP :: Parser a -> Int -> Parser [a]
repeatP _ 0 = return []
repeatP p n = (:) <$> p <*> repeatP p (n - 1)

-- parse a list [a] delimited by separators b
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = p `sepby1` sep

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = (:) <$> p <*> many (sep *> p)

-- chain left/right associative binary operators
-- eg for int addition: chainl (many1 $ char isDigit) (char "+") (return 0)
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op fallback = (p `chainl1` op) <|> return fallback
chainr p op fallback = (p `chainr1` op) <|> return fallback

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a
p `chainr1` op = p >>= rest
  where rest a =  (op <*> pure a <*> (p >>= rest)) <|> return a

space :: Parser String
space = many (satisfy isSpace)

-- parse a string token and discard trailing spaces
symb :: String -> Parser String
symb cs = string cs <* space

-- support LaTeX - if frac is a symbol, then this matches frac and \frac
-- so if frac is a binary function \frac{1}{2} == 1/2
latexSymb :: String -> Parser String
latexSymb cs = symb cs <|> symb ('\\' : cs)

digit :: Parser Int
digit = fmap (\c -> ord c - ord '0') (satisfy isDigit)

integer :: Parser Number
integer = unaryMinus $ fmap (NumZ . read) (many1 (satisfy isDigit))

-- adds unary minus support to an existing parser (supports multiple -'s)
unaryMinus :: Parser Number -> Parser Number
unaryMinus p = many (char '-') >>= bool p (fmap negate p) . odd . length

floatingPoint :: Parser Number
floatingPoint = (do
  first <- integer <|> return 0
  char '.'
  num_zeros <- fmap length (many1 (char '0')) <|> return 0
  second <- integer <|> return 0
  let num_digits = NumZ . toInteger $ num_zeros + fromInteger (truncateDoubleInteger (logBase 10 $ toDouble second) + 1)
  return $ toR (first + second / 10 ** num_digits))
  <|> integer

floatingPointExponent :: Parser Number
floatingPointExponent = (do
  x <- floatingPoint
  char 'e'
  e <- unaryMinus floatingPoint
  return (x * 10**e)) <|> floatingPoint

number :: Parser Number
number = floatingPointExponent <* space

-- apply a parser and discard leading spaces
apply :: Parser a -> String -> [(a, String)]
apply p = parse (space *> p)

-- apply a parser only if a different parser would fail, without consuming the string
-- ex: (notahead (symb "++")) *> symb "+"  matches + only if there is no ++
notahead :: Parser a -> Parser ()
notahead p = Parser (\cs ->
  if null (apply p cs)
    then return ((), cs)
    else mempty)
