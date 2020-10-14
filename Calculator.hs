
module Calculator where

import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>), empty)
import Control.Monad (unless)
import Data.List (filter, elemIndex)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import GHC.Real
import Number

type UserFunction = String
type Variable = (String, Number)
-- pass functions and hash-map of variables around
type CalcState = ([UserFunction], [Variable])
emptyState :: CalcState
emptyState = ([], [])

expr :: CalcState -> Parser Number

expr st = infix_functions (postfix_function st <|> subexpr st)

subexpr st = unaryMinus $ number <|> unary_function st <|> constant <|> binary_function st
  <|> user_function st <|> user_variable st <|> brackets st

constant = foldr1 (<|>) $ map
  (\(cs, val) -> symb cs *> return val)
  [("pi", pi), ("\\pi", pi), ("e", exp 1)]

brackets st = foldr1 (<|>) $ map
  (\(l, r) -> symb l *> (expr st) <* symb r)
  [("(",")"), ("[","]"), ("{","}"),
  ("\\left(","\\right)"), ("\\left[","\\right]"), ("\\left{","\\right}")]

unary_function st = foldr1 (<|>) $ map
  (\(cs, f) -> latexSymb cs *> subexpr st >>= return . f)
  [("sin", sin), ("cos", cos), ("tan", tan), ("sqrt", sqrt), ("exp", exp),
  ("ln", log), ("log", logBase 10), ("sinh", sinh), ("cosh", cosh), ("tanh", tanh),
  -- expose toR, toZ, toQ so you can get fraction -> decimal, etc
  ("R", toR), ("Q", toQ), ("Z", toZ),
  ("asin", asin), ("arcsin", asin), ("sin^-1", asin), ("sin^{-1}", asin),
  ("acos", acos), ("arccos", acos), ("cos^-1", acos), ("cos^{-1}", acos),
  ("atan", atan), ("arctan", atan), ("tan^-1", atan), ("tan^{-1}", atan),
  ("asinh", asinh), ("arcsinh", asinh), ("sinh^-1", asinh), ("sinh^{-1}", asinh),
  ("acosh", acosh), ("arccosh", acosh), ("cosh^-1", acosh), ("cosh^{-1}", acosh),
  ("atanh", atanh), ("arctanh", atanh), ("tanh^-1", atanh), ("tanh^{-1}", atanh)]

binary_function st = foldr1 (<|>) $ map
  (\(cs, f) -> latexSymb cs *> (f <$> subexpr st <*> subexpr st))
  [("frac", (/)), ("max", max), ("min", min), ("log_", logBase),
  ("nCr", choose), ("nPr", perms), ("gcd", numGCD), ("Ql", limit_denominator)]

-- |Infix functions are implemented by chaining expressions with parsers for
-- infix functions which produce Parser (Number -> Number -> Number)
-- allowing chain to use applicative syntax to evaluate in-place, so no syntax
-- tree is needed.
infix_functions ex = ex `chainr1` pow `chainl1` comb `chainl1` mul `chainl1` implicitmul `chainl1` add where
  pow = (symb "**" <|> symb "^") *> return (**)
  comb = ((symb "C" <|> symb "choose") *> return choose)
      <|> (symb "P"                    *> return perms)
  mul = ((symb "*" <|> symb "\\times") *> return (*)) <|> (symb "/" *> return (/))
  add =  (symb "+"                     *> return (+)) <|> (symb "-" *> return (-))
  -- implicitly multiply any two consecutive expressions without an operation
  implicitmul = notahead add *> return (*)

postfix_function st = foldr1 (<|>) $ map
  (\(cs, f) -> subexpr st <* symb cs >>= return . f)
  [("!", numFactorial)]

-- |This processes a string eg "f x y = 1/x + ln y" and lets you then write
-- "f {expression} {expression}" anywhere inside an expression
user_function :: CalcState -> Parser Number
user_function st@(fns, vars) = foldr (<|>) empty (map user_func fns) where
  user_func :: UserFunction -> Parser Number
  user_func template =
    let eq = fromJust (elemIndex '=' template)
        arg_names = map (:[]) $ filter (not . isSpace) (take eq template)
        -- We add a + 0x to the end of the function to force it to use our
        -- NumFD automatic differentiation type instead of the others.
        -- (Otherwise f x = const means f' x = const, but +NumFD coerces
        -- the constant to NumFD and gives f' x = 0 as desired)
        second = drop (eq + 1) template ++ " + 0*" ++ (arg_names !! 1)
        args = repeatP (subexpr st) (length arg_names - 1) :: Parser [Number]
        subParse :: [Number] -> Parser Number
        subParse nums =
          let vars' = zip (tail arg_names) nums
              st' = (fns, vars'++vars)
              x = apply (expr st') second
          in case x of
            [] -> mempty
            otherwise -> return . toDisplay . fst . head $ x

        normalFunc =
          symb (head arg_names) *> args >>= subParse

        unaryDerivativeFunc =
          if length arg_names /= 2 then empty else
            symb (head arg_names ++ "'") *> args >>= subParse . (map toFD)

        partialDerivativeFunc =
          foldr1 (<|>) (map partial (zip (tail arg_names) [0..]))
          where
            partial (arg, i) =
              let token = "d" ++ head arg_names ++ "/d" ++ arg
              in symb token *> (args >>= subParse . (mapNth i toFD))

    in partialDerivativeFunc <|> unaryDerivativeFunc <|> normalFunc

  -- apply a map function to only the nth element of a list
  mapNth :: Int -> (a -> a) -> [a] -> [a]
  mapNth _ _ [] = []
  mapNth 0 f (x:xs) = f x : xs
  mapNth n f (x:xs) = x : mapNth (n - 1) f xs

user_variable :: CalcState -> Parser Number
user_variable (_, vars) = foldr (<|>) empty
  (map (\(cs, n) -> symb cs *> pure n) vars)

prettyExpr :: (String, CalcState) -> (String, CalcState)
prettyExpr (s, st@(fns, vars))
  -- semicolons split multiple expressions in one line
  | ';' `elem` s =
    let n = fromJust (elemIndex ';' s)
        fst'  = take n s; snd' = drop (n + 1) s
        (_, st') = prettyExpr (fst', st)
    in prettyExpr (snd', st')
  -- equals means a variable or function definition
  | '=' `elem` s =
    let idx = fromJust (elemIndex '=' s)
        sig = take idx s
        sigChars s = filter (not . isSpace) s
        second = drop (idx + 1) s
    in case (length $ sigChars sig) of
      0 -> ("error: invalid = sign", st)

      -- define variable
      1 ->
        let name = take 1 (sigChars sig)
            nums = apply (expr st) second
            vars' = filter (\(a,b) -> a /= name) vars
        in case nums of
          [] -> ("No results", st)
          ns -> ("defined "++sig, (fns, (name, fst (head ns)):vars'))

      -- define function
      _ ->
        let fns' = filter (\s -> take 1 (sigChars s) /= take 1 (sigChars sig)) fns
        in ("defined "++sig, (s:fns', vars))

  -- normal expression
  | otherwise = (num s st, st)

  where
    num str st = let x = apply (expr st) str in
      if (length x == 0)
        then "No results"
        else if (length (snd (x !! 0)) /= 0)
          then "Invalid character encountered: " ++ snd (head x)
          else show (fst (head x))
