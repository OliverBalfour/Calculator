
import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.List (filter)

expr :: Parser Double
-- todo: data structure storing operations along with their fixity and precedence
-- so we can dynamically add infixr7, prefix9 etc functions/operators
-- note: same precedence needs foldr1 (<|>), different needs foldr1 (chain(l or r)1)
expr = subexpr `chainr1` powop `chainl1` mulop `chainl1` implicitmulop `chainl1` addop

subexpr = number <|> constant <|> unary_function <|> binary_function <|> brackets

constant = foldr1 (<|>) $ map
  (\(cs, val) -> symb cs *> return val)
  [("pi", pi), ("e", exp 1)]

brackets = foldr1 (<|>) $ map
  (\(l, r) -> symb l *> expr <* symb r)
  [("(",")"), ("[","]"), ("{","}"),
  ("\\left(","\\right)"), ("\\left[","\\right]"), ("\\left{","\\right}")]

unary_function = foldr1 (<|>) $ map
  (\(cs, f) -> do
    symb cs
    a <- subexpr
    return (f a))
  [("sin", sin), ("cos", cos), ("tan", tan), ("sqrt", sqrt)]

binary_function = foldr1 (<|>) $ map
  (\(cs, f) -> do
    symb cs
    a <- subexpr
    b <- subexpr
    return (f a b))
  [("\\frac", (/)), ("max", max), ("min", min)]

powop :: Parser (Double -> Double -> Double)
powop = (symb "**" <|> symb "^") *> return (**)

addop :: Parser (Double -> Double -> Double)
addop = (symb "+" *> return (+)) <|> (symb "-" *> return (-))

mulop :: Parser (Double -> Double -> Double)
mulop = ((symb "*" <|> symb "\\times") *> return (*)) <|> (symb "/" *> return (/))

-- after mulop is applied we effectively have numbers with only +- or blanks between
-- eg 1 2 + 3 4 - 5, which we want to turn into 1*2 + 3*4 - 5
implicitmulop :: Parser (Double -> Double -> Double)
implicitmulop = notahead addop *> return (*)

prettyExpr :: String -> String
prettyExpr cs = let x = apply expr cs in
  if (length x == 0)
    then "No results"
    else if (length (snd (x !! 0)) /= 0)
      then "Invalid character encountered: " ++ snd (x !! 0)
      else show $ fst (x !! 0)

test :: IO [()]
test = sequence $ map printFailed (filter (not . testPasses) tests)
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
      -- floating point numbers and scientific notation
      ("10e2", 1000.0),
      ("9e-2", 0.09),
      ("3.1415e4", 31415.0)
      ]

main :: IO ()
main = do
  test
  putStrLn "Calculator"
  interact $ unlines . map prettyExpr . lines
