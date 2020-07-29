
import ParserCombinator
import GHC.Float (int2Double)
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.List (filter)

expr :: Parser Double
expr = factor `chainr1` powop `chainl1` mulop `chainl1` addop
factor = number <|> constant <|> brackets
constant = (symb "pi" *> return pi) <|> (symb "e" *> return (exp 1))
brackets = symb "(" *> expr <* symb ")"

powop :: Parser (Double -> Double -> Double)
powop = (symb "**" <|> symb "^") *> return (**)

addop :: Parser (Double -> Double -> Double)
addop = (symb "+" *> return (+)) <|> (symb "-" *> return (-))

mulop :: Parser (Double -> Double -> Double)
mulop = (symb "*" *> return (*)) <|> (symb "/" *> return (/))

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
      ("2 /1.0 * (3*pi - 4 + 2*2)+e", 6*pi+exp 1)]

main :: IO ()
main = do
  test
  interact $ unlines . map (show . prettyExpr) . lines
