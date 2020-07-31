
import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>), empty)
import Control.Monad (unless)
import Data.List (filter, elemIndex)
import Data.Function (on)
import Data.Maybe (fromJust)
import Number

type UserFunction = ([Number] -> String, String, Int)

expr :: [UserFunction] -> Parser Number
-- todo: data structure storing operations along with their fixity and precedence
-- so we can dynamically add infixr7, prefix9 etc functions/operators
-- note: same precedence needs foldr1 (<|>), different needs foldr1 (chain(l or r)1)

expr fns = infix_functions (postfix_function fns <|> subexpr fns)

subexpr fns = number <|> constant <|> unary_function fns <|> binary_function fns <|> user_function fns <|> brackets fns

constant = foldr1 (<|>) $ map
  (\(cs, val) -> symb cs *> return val)
  [("pi", pi), ("e", exp 1)]

brackets fns = foldr1 (<|>) $ map
  (\(l, r) -> symb l *> (expr fns) <* symb r)
  [("(",")"), ("[","]"), ("{","}"),
  ("\\left(","\\right)"), ("\\left[","\\right]"), ("\\left{","\\right}")]

unary_function fns = foldr1 (<|>) $ map
  (\(cs, f) -> latexSymb cs *> subexpr fns >>= return . f)
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

binary_function fns = foldr1 (<|>) $ map
  (\(cs, f) -> latexSymb cs *> (f <$> subexpr fns <*> subexpr fns))
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

postfix_function fns = foldr1 (<|>) $ map
  (\(cs, f) -> subexpr fns <* symb cs >>= return . f)
  [("!", numFactorial)]

-- source: https://mail.haskell.org/pipermail/haskell-cafe/2008-July/045458.html
replace :: String -> String -> String -> String
replace [] old new = []
replace str old new = loop str
  where
    loop [] = []
    loop str =
      let (prefix, rest) = splitAt n str
      in
        if old == prefix                -- found an occurrence?
        then new ++ loop rest           -- yes: replace it
        else head str : loop (tail str) -- no: keep looking
    n = length old

-- note: function application should be repeated so functions can be partially applied and defined in terms of each other
-- this takes a string eg "f x y = 1/x + ln y" and creates a function that returns eg "1/{x} + ln {y}" given [x, y]
-- this means we can define functions by simple string manipulation
createFunction :: String -> UserFunction
createFunction template =
  let eq = fromJust $ elemIndex '=' template
      first = take eq template -- "f x y "
      second = drop (eq + 1) template -- " 1/x + ln y"
      names = filter ((>0) . length) (words first) -- ["f", "x", "y"]
      replaceTemplate temp (num, name) = replace temp name (show num)
      fn numbers = foldl replaceTemplate second (zip numbers (tail names))
  in (fn, head names, length names - 1) -- eg ([x, y]->String, "f")

user_function :: [UserFunction] -> Parser Number
user_function fns = foldr (<|>) empty (map user_function fns) where
  user_function :: UserFunction -> Parser Number
  -- ([Number] -> String, String, Int)
  user_function (fn, name, no_args) =
    let args = repeatP (subexpr fns) no_args :: Parser [Number]
        subParse = fst . head . apply (expr fns) :: String -> Number
    in symb name *> args >>= return . subParse . fn

prettyExpr :: (String, [UserFunction]) -> (String, [UserFunction])
prettyExpr (s, fns)
  | '=' `elem` s = let fn@(_,e,_) = createFunction s in ("defined "++e, fn:fns)
  | otherwise = (num s fns, fns)
  where
    num str fns = let x = apply (expr fns) str in
      if (length x == 0)
        then "No results"
        else if (length (snd (x !! 0)) /= 0)
          then "Invalid character encountered: " ++ snd (head x)
          else show (fst (head x))

run :: [UserFunction] -> IO ()
run fns = do
  s <- getLine
  let (out, fns') = prettyExpr (s, fns)
  putStrLn out
  run fns'

main :: IO ()
main = do
  test
  putStrLn "Calculator"
  run []

test :: IO [()]
test = mapM printFailed (filter (not . testPasses) tests)
  where
    printFailed = \(cs, out) -> putStrLn $ "error: " ++ cs ++ " == " ++ show out
    testPasses (cs, out) = let res = apply (expr []) cs in
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
