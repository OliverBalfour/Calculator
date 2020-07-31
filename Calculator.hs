
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

subexpr st = number <|> unary_function st <|> constant <|> binary_function st
  <|> user_function st <|> user_variable st <|> brackets st

constant = foldr1 (<|>) $ map
  (\(cs, val) -> symb cs *> return val)
  [("pi", pi), ("e", exp 1)]

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
  ("nCr", choose), ("nPr", perms), ("gcd", numGCD)]

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
        second = drop (eq + 1) template
        args = repeatP (subexpr st) (length arg_names - 1) :: Parser [Number]
        subParse :: [Number] -> Parser Number
        subParse nums =
          let vars' = zip (tail arg_names) nums
              st' = (fns, vars'++vars)
              x = apply (expr st') second in case x of
            [] -> mempty
            otherwise -> return . fst . head $ x
    in symb (head arg_names) *> args >>= subParse

user_variable :: CalcState -> Parser Number
user_variable (_, vars) = foldr (<|>) empty
  (map (\(cs, n) -> symb cs *> pure n) vars)

prettyExpr :: (String, CalcState) -> (String, CalcState)
prettyExpr (s, st@(fns, vars))
  | ';' `elem` s =
    let n = fromJust (elemIndex ';' s)
        fst'  = take n s; snd' = drop (n + 1) s
        (_, st') = prettyExpr (fst', st)
    in prettyExpr (snd', st')
  | '=' `elem` s =
    let sig = take (fromJust (elemIndex '=' s)) s in
    ("defined "++sig, (s:fns, vars))
  | otherwise = (num s st, st)
  where
    num str fns = let x = apply (expr fns) str in
      if (length x == 0)
        then "No results"
        else if (length (snd (x !! 0)) /= 0)
          then "Invalid character encountered: " ++ snd (head x)
          else show (fst (head x))

run :: CalcState -> IO ()
run fns = do
  s <- getLine
  let (out, fns') = prettyExpr (s, fns)
  putStrLn out
  run fns'

main :: IO ()
main = do
  -- test
  putStrLn "Calculator"
  run emptyState

-- test :: IO [()]
-- test = mapM printFailed (filter testFails tests) where
--   printFailed = \(cs, out) -> putStrLn $ "error: " ++ cs ++ " /= " ++ show out
--   testFails (cs, out) = show out /= fst (prettyExpr (cs, emptyState))
--   tests = [
--     ("1+2", NumZ 3), ("1.0 + 2.1", NumQ (31:%10)), ("   0. + 0.1   + 0.2 -  .3", NumZ 0), -- basic arithmetic, spaces
--     ("5 + -4 - (5 - 4)", NumZ 0), -- unary minus
--     ("(3 + 4) / (5 - 4)", NumZ 7), ("[5] - {4} * [3 - 2]", NumZ 1), ("5 * (3 - 2)", NumZ 5), -- brackets
--     ("2 /1.0 * (3*pi - 4 + 2*2)+e", NumR$6*pi+exp 1), -- constants
--     ("3  - 2 * 3", NumZ(-3)), ("(2**3)^4", NumZ 4096), ("2^3^2",NumZ 512), ("2/3*3", NumZ 2), -- associativity
--     ("3\\times \\left(\\frac{5}{9}\\right)^2*\\left(\\frac{4}{9}\\right)^1", NumQ (100:%243)), -- latex
--     ("2pi(3 + 4)", NumR$14.0 * pi), ("5^2\\frac(1) 5", NumZ 5), ("2 / 3(2 + 4)", NumZ 4), -- implicit mult
--     ("sin 0", NumR 0.0), ("5max 7 4", NumZ 35), ("max max max 1 20 3 4", NumZ 20), -- functions
--     ("\\frac{1}{2}", NumQ (1:%2)), ("ln e", NumR 1.0), ("log 100", NumR 2.0), ("\\log_2 8", NumR 3.0),
--     ("10e2", NumZ 1000), ("9e-2", NumQ (9:%100)), ("3.1415e4", NumZ 31415), -- floating point, scientific notation
--     ("5!", NumZ 120), ("10 choose 4", NumZ 210), ("10C6", NumZ 210), -- permutations and combinations
--     ("fx=xx;gx=f(lnx);g(exp2)", NumR 4.0) -- user functions
--     ]
