
import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.List (filter)
import Data.Function (on)
import Number

expr :: Parser Number
-- todo: data structure storing operations along with their fixity and precedence
-- so we can dynamically add infixr7, prefix9 etc functions/operators
-- note: same precedence needs foldr1 (<|>), different needs foldr1 (chain(l or r)1)

expr = infix_functions (postfix_function <|> subexpr)

subexpr = number <|> constant <|> unary_function <|> binary_function <|> brackets

constant = foldr1 (<|>) $ map
  (\(cs, val) -> symb cs *> return val)
  [("pi", pi), ("e", exp 1)]

brackets = foldr1 (<|>) $ map
  (\(l, r) -> symb l *> expr <* symb r)
  [("(",")"), ("[","]"), ("{","}"),
  ("\\left(","\\right)"), ("\\left[","\\right]"), ("\\left{","\\right}")]

unary_function = foldr1 (<|>) $ map
  (\(cs, f) -> latexSymb cs *> subexpr >>= return . f)
  [("sin", sin), ("cos", cos), ("tan", tan), ("sqrt", sqrt), ("exp", exp),
  ("ln", log), ("log", logBase 10), ("sinh", sinh), ("cosh", cosh), ("tanh", tanh),
  ("asin", asin), ("arcsin", asin), ("sin^-1", asin), ("sin^{-1}", asin),
  ("acos", acos), ("arccos", acos), ("cos^-1", acos), ("cos^{-1}", acos),
  ("atan", atan), ("arctan", atan), ("tan^-1", atan), ("tan^{-1}", atan),
  ("asinh", asinh), ("arcsinh", asinh), ("sinh^-1", asinh), ("sinh^{-1}", asinh),
  ("acosh", acosh), ("arccosh", acosh), ("cosh^-1", acosh), ("cosh^{-1}", acosh),
  ("atanh", atanh), ("arctanh", atanh), ("tanh^-1", atanh), ("tanh^{-1}", atanh)]

binary_function = foldr1 (<|>) $ map
  (\(cs, f) -> latexSymb cs *> (f <$> subexpr <*> subexpr))
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

postfix_function = foldr1 (<|>) $ map
  (\(cs, f) -> subexpr <* symb cs >>= return . f)
  [("!", numFactorial)]

prettyExpr :: String -> String
prettyExpr cs = let x = apply expr cs in
  if (length x == 0)
    then "No results"
    else if (length (snd (x !! 0)) /= 0)
      then "Invalid character encountered: " ++ snd (x !! 0)
      else show $ fst (x !! 0)

test :: IO [()]
test = mapM printFailed (filter (not . testPasses) tests)
  where
    printFailed = \(cs, out) -> putStrLn $ "error: " ++ cs ++ " == " ++ show out
    testPasses (cs, out) = let res = apply expr cs in
      if (length res == 0)
        then False
        else (fst (res !! 0)) `dblEq` out
    a `dblEq` b = abs (a - b) < 0.0001
    tests = [
      -- basic arithmetic, spaces
      ("1+2", 3.0),
      ("1.0 + 2.1", 3.1),
      ("   0. + 0.1   + 0.2 -  .3", 0.0),
      -- unary minus
      ("5 + -4 - (5 - 4)", 0.0),
      -- brackets
      ("(3 + 4) / (5 - 4)", 7.0),
      ("[5] - {4} * [3 - 2]", 1.0),
      ("5 * (3 - 2)", 5.0),
      -- constants
      ("2 /1.0 * (3*pi - 4 + 2*2)+e", 6*pi+exp 1),
      -- associativity
      ("3  - 2 * 3", -3.0),
      ("(2**3)^4", 4096.0),
      ("3^4^(1/2.00)",9.0),
      ("2/3*3", 2.0),
      -- latex
      ("5\\times\\left(4-2\\right)", 10.0),
      ("3\\times \\left(\\frac{5}{9}\\right)^2*\\left(\\frac{4}{9}\\right)^1", 100.0/243.0),
      -- multiplication without symbol
      ("2pi(3 + 4)", 14.0 * pi),
      ("5^2\\frac(1) 5", 5.0),
      ("2 / 3(2 + 4)", 4.0), -- equiv. to 2 / 3 * (2 + 4)
      -- functions
      ("sin 0", 0.0),
      ("5max 7 4", 35.0),
      ("max max max 1 20 3 4", 20.0),
      ("\\frac{1}{2}", 0.5),
      ("ln e", 1.0),
      ("log 100", 2.0),
      ("\\log_2 8", 3.0),
      -- floating point numbers and scientific notation
      ("10e2", 1000.0),
      ("9e-2", 0.09),
      ("3.1415e4", 31415.0),
      -- permutations and combinations
      ("5!", 120.0),
      ("10 choose 4", 210.0),
      ("10C6", 210.0)
      ]

main :: IO ()
main = do
  test
  putStrLn "Calculator"
  interact $ unlines . map prettyExpr . lines
