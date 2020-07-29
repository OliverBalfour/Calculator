
import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.List (filter)

expr :: Parser Double
expr = factor `chainr1` powop `chainl1` mulop `chainl1` addop
factor = number <|> constant <|> brackets

constant = foldr1 (<|>) $ map
  (\(cs, val) -> symb cs *> return val)
  [("pi", pi), ("e", exp 1)]

brackets = foldr1 (<|>) $ map
  (\(l, r) -> symb l *> expr <* symb r)
  [("(",")"), ("[","]"), ("{","}"),
  ("\\left(","\\right)"), ("\\left[","\\right]"), ("\\left{","\\right}")]

powop :: Parser (Double -> Double -> Double)
powop = (symb "**" <|> symb "^") *> return (**)

addop :: Parser (Double -> Double -> Double)
addop = (symb "+" *> return (+)) <|> (symb "-" *> return (-))

mulop :: Parser (Double -> Double -> Double)
mulop = ((symb "*" <|> symb "\\times") *> return (*)) <|> (symb "/" *> return (/))

prettyExpr :: String -> Double
prettyExpr = fst . (!!0) . apply expr

test :: IO [()]
test = sequence $ map printFailed (filter (not . testPasses) tests)
  where
    printFailed = \(cs, out) -> putStrLn $ "error: " ++ cs ++ " == " ++ show out
    testPasses (cs, out) = (prettyExpr cs) `dblEq` out
    a `dblEq` b = abs (a - b) < 0.0001
    tests = [
      ("1+2", 3.0),
      ("1.0 + 2.1", 3.1),
      ("   0.1   + 0.2 -  0.3", 0.0),
      ("5 * (3 - 2)", 5.0),
      ("2 /1.0 * (3*pi - 4 + 2*2)+e", 6*pi+exp 1),
      ("(2**3)^4", 4096.0),
      ("3^4^(1/2.00)",9.0),
      ("(3 + 4) / (5 - 4)", 7.0),
      ("[5] - {4} * [3 - 2]", 1.0),
      ("5\\times\\left(4-2\\right)", 10.0)]

main :: IO ()
main = do
  test
  putStrLn "Calculator"
  interact $ unlines . map (show . prettyExpr) . lines
